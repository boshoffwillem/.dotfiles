;;; go.el --- Go tree-sitter + LSP (gopls) support  -*- lexical-binding: t; -*-

;; Same "-maybe" pattern as rust.el and yaml.el, with one difference: core
;; Emacs 31 ships `go-ts-mode-maybe', but when the grammar is missing it falls
;; back to `fundamental-mode', which gives no highlighting. So the fallback
;; here is the `go-mode' package instead.
;;
;; lsp-mode's built-in gopls client (clients/lsp-go.el) activates on go-mode,
;; go-ts-mode and go-dot-mod-mode. It needs `gopls' on PATH:
;;   go install golang.org/x/tools/gopls@latest
;;
;; Formatting goes through apheleia (gofmt), which already maps go-mode and
;; go-ts-mode in its own defaults, so nothing is needed in init.el.
;;
;; One-time setup per machine: M-x treesit-install-language-grammar RET go

(require 'zig-treesit (locate-user-emacs-file "zig-treesit"))
(require 'lsp-setup (locate-user-emacs-file "lsp-setup"))

;; `go install' puts tools in ~/go/bin. That directory isn't on the login-shell
;; PATH that exec-path-from-shell copies into Emacs, so lsp-mode would never
;; find gopls. Appended rather than prepended, like the git usr/bin block in
;; init.el, so anything already found earlier on the path still wins.
(let ((gobin (expand-file-name "go/bin" "~")))
  (when (file-directory-p gobin)
    (add-to-list 'exec-path gobin t)))

(my/treesit-register-grammar
 'go "https://github.com/tree-sitter/tree-sitter-go" "master")

(use-package go-mode
  :straight t)

(defun my/go-mode-maybe ()
  "Use tree-sitter `go-ts-mode' if its grammar is installed, else `go-mode'."
  (if (treesit-language-available-p 'go)
      (go-ts-mode)
    (go-mode)))

;; After the `go-mode' use-package above, so these entries land in front of
;; the ones go-mode's autoloads and core's `-maybe' entries add.
(add-to-list 'auto-mode-alist '("\\.go\\'" . my/go-mode-maybe))
(add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-dot-mod-mode))

(defun my/go-setup ()
  "Indent Go with real tabs, the way gofmt writes it."
  ;; init.el defaults to spaces, and gofmt output is tabs.
  (setq-local indent-tabs-mode t)
  ;; go-ts-mode indents by `go-ts-mode-indent-offset' columns (8 by default).
  ;; Match it to `tab-width' so one indent level is one tab.
  (setq-local go-ts-mode-indent-offset tab-width))

(dolist (hook '(go-ts-mode-hook go-mode-hook))
  (add-hook hook #'my/go-setup)
  (add-hook hook #'lsp-deferred))
(add-hook 'go-dot-mod-mode-hook #'lsp-deferred)

;;; go.el ends here
