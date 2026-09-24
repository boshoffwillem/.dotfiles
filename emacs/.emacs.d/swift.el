;;; swift.el --- Swift major mode + LSP (sourcekit-lsp) support  -*- lexical-binding: t; -*-

;; sourcekit-lsp ships with Xcode / the Swift toolchain (/usr/bin/sourcekit-lsp
;; on macOS is an xcrun shim that resolves to the active toolchain), so
;; there's nothing to install separately. lsp-mode doesn't bundle a Swift
;; client itself; the separate `lsp-sourcekit' package registers it, and it
;; has to be loaded (not just installed) for lsp-mode to know about it.
;;
;; No tree-sitter mode here: core Emacs doesn't ship `swift-ts-mode', and
;; `swift-mode' is the mature, commonly used package.

(require 'lsp-setup (locate-user-emacs-file "lsp-setup"))

(use-package swift-mode
  :straight t
  ;; :hook (swift-mode . lsp-deferred)
  )

;; (use-package lsp-sourcekit
;;   :straight t
;;   :after lsp-mode)

;;; swift.el ends here
