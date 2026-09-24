;;; rust.el --- Rust tree-sitter + LSP (rust-analyzer) support  -*- lexical-binding: t; -*-

;; Same "-maybe" pattern as csharp.el: core Emacs ships `rust-ts-mode', but
;; invoking it without the grammar installed signals an error, so fall back
;; to the `rust-mode' package when the grammar isn't there yet.
;;
;; lsp-mode's built-in rust-analyzer client (clients/lsp-rust.el) is the
;; default Rust server and activates on both modes -- just needs
;; `rust-analyzer' on PATH (`rustup component add rust-analyzer').
;;
;; Formatting goes through apheleia (rustfmt), set up in init.el.
;;
;; One-time setup per machine: M-x treesit-install-language-grammar RET rust

(require 'zig-treesit (locate-user-emacs-file "zig-treesit"))
(require 'lsp-setup (locate-user-emacs-file "lsp-setup"))

(my/treesit-register-grammar
 'rust "https://github.com/tree-sitter/tree-sitter-rust" "master")

(use-package rust-mode
  :straight t)

(defun my/rust-mode-maybe ()
  "Use tree-sitter `rust-ts-mode' if its grammar is installed, else `rust-mode'."
  (if (treesit-language-available-p 'rust)
      (rust-ts-mode)
    (rust-mode)))

;; After the `rust-mode' use-package above, so this entry lands in front of
;; the one rust-mode's autoloads add.
(add-to-list 'auto-mode-alist '("\\.rs\\'" . my/rust-mode-maybe))

;; (add-hook 'rust-mode-hook #'lsp-deferred)
;; (add-hook 'rust-ts-mode-hook #'lsp-deferred)

;;; rust.el ends here
