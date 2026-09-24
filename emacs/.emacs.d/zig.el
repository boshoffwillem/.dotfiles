;;; zig.el --- Zig major mode + LSP (zls) support  -*- lexical-binding: t; -*-

;; lsp-mode's built-in zls client (clients/lsp-zig.el) activates on the
;; "zig" language-id, which lsp-mode maps `zig-mode' to. If `zls' isn't on
;; PATH, lsp-mode offers to download a matching release itself.
;;
;; No tree-sitter mode here: core Emacs doesn't ship `zig-ts-mode', and
;; `zig-mode' is the official package from the Zig project.
;;
;; `zig-mode' runs `zig fmt' on save by default, but apheleia (init.el)
;; already maps `zig-mode' to `zig fmt' too -- turn zig-mode's own hook off
;; so files aren't formatted twice.

(require 'lsp-setup (locate-user-emacs-file "lsp-setup"))

(use-package zig-mode
  :straight t
  :custom
  (zig-format-on-save nil)
  :hook (zig-mode . lsp-deferred)
  )

;;; zig.el ends here
