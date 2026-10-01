;;; proto.el --- Protocol Buffers (.proto) support  -*- lexical-binding: t; -*-

;; Same "-maybe" pattern as csharp.el/rust.el/yaml.el, except neither core
;; Emacs nor MELPA currently ships a maintained `protobuf-ts-mode': the
;; emacsattic one (MELPA "protobuf-ts-mode") is proto3-only and effectively
;; abandoned, so this file carries its own minimal tree-sitter major mode
;; instead (font-lock, imenu, indentation, defun navigation -- the treesit.el
;; basics).  It activates against the `proto' grammar
;; (coder3101/tree-sitter-proto, actively maintained) and falls back to
;; `protobuf-mode' from the upstream protocolbuffers/protobuf repo when the
;; grammar isn't built yet.
;;
;; lsp-mode's bufls/buf clients (clients/lsp-bufls.el) activate on the
;; "protobuf" language-id; lsp-mode already maps `protobuf-mode' to it, this
;; file adds the same mapping for `my/protobuf-ts-mode'.  Either server
;; works -- bufls (`go install
;; github.com/bufbuild/buf-language-server/cmd/bufls@latest' or `brew
;; install bufls') or the Buf CLI's built-in one (`buf lsp serve');
;; lsp-mode picks whichever binary is on PATH (buf wins: priority 0 vs
;; bufls' -1).
;;
;; Folding: treesit-fold has no entry for a protobuf mode, so one is added
;; below -- message/enum bodies and block literals fold via the generic
;; `treesit-fold-range-seq' (brace-delimited sequences, like C++'s
;; declaration_list), comments fold C-style.
;;
;; Formatting: `clang-format' formats .proto natively (proto LexerKind
;; landed in clang 15), but that isn't wired through apheleia here --
;; `buf format' would be the alternative; wire either into apheleia's
;; `apheleia-mode-alist' in init.el when wanted, or run them manually.
;;
;; One-time setup per machine: M-x treesit-install-language-grammar RET proto
;; (uses the recipe below; see zig-treesit.el for why CC/C++ point at zig on
;; Windows).

(require 'zig-treesit (locate-user-emacs-file "zig-treesit"))
(require 'lsp-setup (locate-user-emacs-file "lsp-setup"))

(my/treesit-register-grammar
 'proto "https://github.com/coder3101/tree-sitter-proto" "master" "src")

;; The classic upstream major mode (Emacs still has no built-in proto mode):
;; syntax highlighting + indentation.  It isn't packaged on MELPA/GNU ELPA
;; on its own, so straight pulls the single file out of the protobuf
;; monorepo.
(use-package protobuf-mode
  :straight (protobuf-mode :type git :host github
                           :repo "protocolbuffers/protobuf"
                           :files ("editors/protobuf-mode.el")))

(defvar my/protobuf-ts-mode-indent-offset 2
  "Number of spaces for each indentation step in `my/protobuf-ts-mode'.")

(defvar my/protobuf-ts-mode--keywords
  '("syntax" "edition" "package" "import" "weak" "public" "local"
    "option" "message" "enum" "service" "rpc" "returns" "stream"
    "extend" "extensions" "to" "max" "reserved" "oneof" "map"
    "optional" "repeated" "required"))

(defvar my/protobuf-ts-mode--indent-rules
  `((proto
     ((node-is ")") parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((parent-is "service") parent-bol my/protobuf-ts-mode-indent-offset)
     ((parent-is "rpc") parent-bol my/protobuf-ts-mode-indent-offset)
     ((parent-is "message_body") parent-bol my/protobuf-ts-mode-indent-offset)
     ((parent-is "enum_body") parent-bol my/protobuf-ts-mode-indent-offset)
     ((parent-is "oneof") parent-bol my/protobuf-ts-mode-indent-offset))))

(defun my/protobuf-ts-mode--defun-name (node)
  "Return the defun name of NODE."
  (treesit-node-text (treesit-search-subtree node "^identifier$" nil t) t))

(define-derived-mode my/protobuf-ts-mode prog-mode "Protocol-Buffers"
  "Major mode for editing Protocol Buffers files, tree-sitter based."
  (when (treesit-ready-p 'proto)
    (treesit-parser-create 'proto)

    ;; Comments
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    (setq-local comment-start-skip "//+\\s-*")

    ;; Font-lock
    (setq-local treesit-font-lock-settings
                (treesit-font-lock-rules
                 :language 'proto
                 :feature 'comment
                 '((comment) @font-lock-comment-face)

                 :language 'proto
                 :feature 'keyword
                 `([,@my/protobuf-ts-mode--keywords] @font-lock-keyword-face)

                 :language 'proto
                 :feature 'string
                 '((string) @font-lock-string-face)

                 :language 'proto
                 :feature 'number
                 '((int_lit) @font-lock-number-face
                   (float_lit) @font-lock-number-face)

                 :language 'proto
                 :feature 'type
                 '((service_name (identifier) @font-lock-type-face)
                   (message_name (identifier) @font-lock-type-face)
                   (enum_name (identifier) @font-lock-type-face)
                   (package (full_ident) @font-lock-type-face)
                   (key_type) @font-lock-type-face
                   (type) @font-lock-type-face
                   (message_or_enum_type) @font-lock-type-face
                   "map" @font-lock-type-face)

                 :language 'proto
                 :feature 'function
                 '((rpc (rpc_name (identifier) @font-lock-function-name-face)))

                 :language 'proto
                 :feature 'variable
                 '((identifier) @font-lock-variable-name-face)))
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (keyword string)
                  (number type function variable)))

    ;; Imenu
    (setq-local treesit-simple-imenu-settings
                `(("Service" "\\`service_name\\'" nil nil)
                  ("RPC" "\\`rpc_name\\'" nil nil)
                  ("Message" "\\`message_name\\'" nil nil)
                  ("Enum" "\\`enum_name\\'" nil nil)))

    ;; Indent
    (setq-local treesit-simple-indent-rules my/protobuf-ts-mode--indent-rules)

    ;; Navigation
    (setq-local treesit-defun-type-regexp
                (rx string-start
                    (or "service" "rpc" "message" "enum")
                    string-end))
    (setq-local treesit-defun-name-function #'my/protobuf-ts-mode--defun-name)

    (treesit-major-mode-setup)))

(defun my/protobuf-mode-maybe ()
  "Use tree-sitter `my/protobuf-ts-mode' if the grammar is installed, else `protobuf-mode'."
  (if (treesit-language-available-p 'proto)
      (my/protobuf-ts-mode)
    (protobuf-mode)))

;; After the `protobuf-mode' use-package above, so this entry lands in front
;; of anything protobuf-mode's autoloads may add.  (.proto2 filenames are
;; rare legacy leftovers, but they'd fall through to fundamental-mode
;; without an explicit entry.)
(add-to-list 'auto-mode-alist '("\\.proto2?\\'" . my/protobuf-mode-maybe))

;; lsp-mode maps `protobuf-mode' -> "protobuf" language-id itself; extend
;; that to the tree-sitter mode (the built-in bufls/buf clients key off the
;; language-id, so this is what actually makes LSP work there).
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(my/protobuf-ts-mode . "protobuf")))

;; Folding: treesit-fold's `treesit-fold-range-alist' has no protobuf entry;
;; message/enum bodies, oneofs and block literals fold like C-style brace
;; blocks, comments like C-family ones.
(with-eval-after-load 'treesit-fold
  (add-to-list 'treesit-fold-range-alist
               '(my/protobuf-ts-mode
                 . ((message_body . treesit-fold-range-seq)
                    (enum_body    . treesit-fold-range-seq)
                    (block_lit    . treesit-fold-range-seq)
                    (comment      . treesit-fold-range-c-like-comment)))))

(add-hook 'my/protobuf-ts-mode-hook #'lsp-deferred)

;;; proto.el ends here