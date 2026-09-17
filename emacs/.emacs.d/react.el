;;; react.el --- React JSX/TSX support  -*- lexical-binding: t; -*-

;; Unlike Vue SFCs, .jsx/.tsx aren't multi-language region files -- they're
;; plain JS/TS with JSX literals parsed by the *same* grammar, so they don't
;; need web-mode's region splitting. Emacs core ships tree-sitter major modes
;; for exactly this (`typescript-ts-mode', `tsx-ts-mode', both deriving from
;; `typescript-ts-base-mode'), and lsp-mode's typescript-language-server
;; client already targets them, so no external package is needed here --
;; only the tree-sitter grammars themselves.
;;
;; Core Emacs already maps .ts -> `typescript-ts-mode-maybe' and
;; .tsx -> `tsx-ts-mode-maybe' (both fall back to `fundamental-mode' if the
;; grammar isn't installed yet, rather than erroring). .jsx has no such
;; built-in mapping, so it's added below; the tsx grammar parses plain JSX
;; fine, so .jsx reuses `tsx-ts-mode-maybe' rather than a separate grammar.
;;
;; One-time setup per machine: M-x treesit-install-language-grammar RET tsx,
;; then again RET typescript (uses the recipe below, no prompting needed).
;; Grammar build policy (zig-as-compiler on Windows, etc.) lives in
;; zig-treesit.el, shared with any other tree-sitter grammar this config
;; registers.
;;
;; lsp-mode's typescript-language-server client (ts-ls) activates by
;; filename regex (.js/.jsx/.ts/.tsx/.vue), not by major-mode, so it works
;; the same whether a buffer is in `typescript-ts-mode' or `tsx-ts-mode' --
;; just hook `lsp-deferred' onto both.
;;
;; ts-ls also depends on a second npm package, `typescript' itself, for the
;; `tsserver' binary it drives (lsp-javascript.el's `lsp-clients-typescript-server-path').
;; TypeScript 7 dropped tsserver entirely (no lib/tsserver.js at all --
;; part of the native/Corsa rewrite), so an unpinned install pulls whatever
;; is latest and every LSP startup fails with "The package typescript is not
;; installed.  Unable to find tsserver", even though `typescript-language-server'
;; itself installed fine. Re-register the dependency pinned to the last
;; tsserver-capable release; `lsp-dependency' just overwrites lsp-mode's
;; global dependency table, so this has to run after lsp-javascript.el
;; installs its own unpinned registration.
(with-eval-after-load 'lsp-javascript
  (lsp-dependency 'typescript
                   '(:system "tsserver")
                   '(:npm :package "typescript" :path "tsserver" :version "5.7.3")))

(require 'frontend (locate-user-emacs-file "frontend"))
(require 'zig-treesit (locate-user-emacs-file "zig-treesit"))
(require 'lsp-setup (locate-user-emacs-file "lsp-setup"))

(my/treesit-register-grammar
 'tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
(my/treesit-register-grammar
 'typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode-maybe))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode-maybe))

(setq typescript-ts-indent-offset my/frontend-indent-width)

(add-hook 'typescript-ts-mode-hook #'lsp-deferred)
(add-hook 'tsx-ts-mode-hook #'lsp-deferred)

;;; react.el ends here
