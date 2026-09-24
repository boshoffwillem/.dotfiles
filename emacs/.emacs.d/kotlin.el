;;; kotlin.el --- Kotlin major mode + LSP (JetBrains kotlin-lsp) support  -*- lexical-binding: t; -*-

;; lsp-mode's built-in Kotlin client (clients/lsp-kotlin.el, server-id
;; `kotlin-ls') targets fwcd's kotlin-language-server, which is deprecated
;; in favour of JetBrains' official `kotlin-lsp' (`brew install
;; JetBrains/utils/kotlin-lsp'). lsp-mode has no client for that one yet, so
;; register it here with a higher priority than `kotlin-ls' -- lsp-mode
;; then picks it whenever both could activate. `--stdio' is required:
;; without it kotlin-lsp listens on a TCP socket (127.0.0.1:9999) instead.
;;
;; No tree-sitter mode here: core Emacs doesn't ship `kotlin-ts-mode', and
;; `kotlin-mode' is the mature, commonly used package.
;;
;; Formatting goes through apheleia (ktlint on PATH), set up in init.el.

(require 'lsp-setup (locate-user-emacs-file "lsp-setup"))

(use-package kotlin-mode
  :straight t
  ;; :hook (kotlin-mode . lsp-deferred)
  )

;; lsp-mode itself is only loaded lazily (its use-package has a :hook), so
;; defer the registration until it's actually loaded.
;; (with-eval-after-load 'lsp-mode
;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (lsp-stdio-connection '("kotlin-lsp" "--stdio"))
;;     :major-modes '(kotlin-mode kotlin-ts-mode)
;;     :priority 1
;;     :server-id 'kotlin-lsp)))

;;; kotlin.el ends here
