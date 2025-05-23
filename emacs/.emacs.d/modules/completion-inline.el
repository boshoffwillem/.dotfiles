;;; completion-inline.el --- settings for company completions -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

;; (use-package company
;;   :init
;;   (global-company-mode)
;;   :hook
;;   ((emacs-lisp-mode . (lambda ()
;;                         (setq-local company-backends '(company-elisp))))
;;    )
;;   :custom
;;   (setq company-show-quick-access t
;;         company-idle-delay 0
;;         company-echo-delay 0
;;         company-tooltip-limit 20
;;         company-tooltip-idle-delay 0.4
;;         company-show-numbers t
;;         company-dabbrev-downcase nil
;;         company-minimum-prefix-length 1
;;         company-selection-wrap-around t)
;;   )

;; (use-package company-box
;;   :after company
;;   :if (display-graphic-p)
;;   :custom
;;   (company-box-frame-behavior 'point)
;;   (company-box-show-single-candidate t)
;;   (company-box-doc-delay 1)
;;   :hook
;;   (company-mode . company-box-mode))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)           ; Minimum length of prefix for completion
  (corfu-auto-delay 0)            ; No delay for completion
  (corfu-popupinfo-delay '(0.5 . 0.2))  ; Automatically update info popup after that numver of seconds
  (corfu-preview-current 'insert) ; insert previewed candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . corfu-insert))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode) ; Popup completion info
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (setq-local corfu-quit-at-boundary t
                                   corfu-quit-no-match t
                                   corfu-auto nil)
              (corfu-mode))
            nil
            t))

(use-package tabnine
  :commands (tabnine-start-process)
  :hook (prog-mode . tabnine-mode)
  :straight t
  :diminish "⌬"
  :custom
  (tabnine-wait 1)
  (tabnine-minimum-prefix-length 0)
  :hook (kill-emacs . tabnine-kill-process)
  :config
  (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)
  (tabnine-start-process)
  ;; :bind
  ;; (:map  tabnine-completion-map
	;;        ("C-i" . tabnine-accept-completion)
	;;        ;; ("TAB" . tabnine-accept-completion)
	;;        ("M-f" . tabnine-accept-completion-by-word)
	;;        ("M-<return>" . tabnine-accept-completion-by-line)
	;;        ("C-g" . tabnine-clear-overlay)
	;;        ("M-[" . tabnine-previous-completion)
	;;        ("M-]" . tabnine-next-completion))
  )

(provide 'completion-inline)

;;; completion-inline.el ends here
