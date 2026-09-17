;;; lsp-setup.el --- shared lsp-mode setup  -*- lexical-binding: t; -*-

;; Installs and configures lsp-mode once; language files (csharp.el,
;; react.el, vue.el, ...) just add `lsp-deferred' to their own mode hooks --
;; lsp-mode resolves the right server per buffer itself (each client's
;; :activation-fn matches on major-mode/file extension -> LSP languageId),
;; so language files don't register or choose servers themselves.

(use-package yasnippet
  :straight t
  )

(use-package posframe
  :straight t)

(defvar my/lsp-doc-posframe-buffer "*lsp-doc-posframe*"
  "Buffer name used by `my/lsp-doc-posframe-show'.")

(defun my/lsp-doc-posframe--hide ()
  "Hide the LSP doc posframe; self-removing, runs once per popup."
  (posframe-hide my/lsp-doc-posframe-buffer)
  (remove-hook 'post-command-hook #'my/lsp-doc-posframe--hide))

(defun my/lsp-doc-posframe-show ()
  "Show LSP hover doc for the symbol at point in a posframe centered
on the frame.  Closes on the next command -- ESC, clicking
elsewhere, or any other keypress."
  (interactive)
  (let* ((contents (-some->> (lsp--text-document-position-params)
                     (lsp--make-request "textDocument/hover")
                     (lsp--send-request)
                     (lsp:hover-contents)))
         (text (and contents (lsp--render-on-hover-content contents t))))
    (if (or (null text) (string-empty-p (string-trim text)))
        (message "No LSP doc at point")
      (posframe-show my/lsp-doc-posframe-buffer
                      :string (string-trim-right text)
                      :poshandler #'posframe-poshandler-frame-center
                      :border-width 1
                      :border-color "gray50"
                      :internal-border-width 12
                      :max-width 100
                      :max-height 40)
      ;; Defer arming the close hook by one command loop iteration, so the
      ;; `K' press that opened the popup doesn't immediately close it.
      (run-at-time 0 nil
                   (lambda () (add-hook 'post-command-hook #'my/lsp-doc-posframe--hide))))))

(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-enable-file-watchers nil) ; watching every project file is slow on large repos
  (lsp-response-timeout 60)
  (lsp-idle-delay 0.6)
  (lsp-enable-folding t)
  (lsp-enable-snippet t) ; expanded via yasnippet, already installed
  (lsp-enable-symbol-highlighting t)
  :config
  (define-key evil-normal-state-map (kbd "gd") 'lsp-find-definition)
  (define-key evil-normal-state-map (kbd "gi") 'lsp-find-implementation)
  (define-key evil-normal-state-map (kbd "gr") 'lsp-find-references)
  ;; Only shadow the global `K' -> `eldoc' binding inside lsp-mode buffers.
  (evil-define-key 'normal lsp-mode-map (kbd "K") #'my/lsp-doc-posframe-show)
  :hook
  (lsp-mode . lsp-enable-which-key-integration))

(provide 'lsp-setup)
;;; lsp-setup.el ends here
