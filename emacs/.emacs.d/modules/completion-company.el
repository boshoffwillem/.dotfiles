;;; completion-company.el --- settings for company completions -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

(use-package company
  :init
  (global-company-mode)
  :hook
  ((emacs-lisp-mode . (lambda ()
                        (setq-local company-backends '(company-elisp))))
   )
  :custom
  (setq company-show-quick-access t
        company-idle-delay 0
        company-echo-delay 0
        company-tooltip-limit 20
        company-tooltip-idle-delay 0.4
        company-show-numbers t
        company-dabbrev-downcase nil
        company-minimum-prefix-length 1
        company-selection-wrap-around t)
  )

(use-package company-box
  :after company
  :if (display-graphic-p)
  :custom
  (company-box-frame-behavior 'point)
  (company-box-show-single-candidate t)
  (company-box-doc-delay 1)
  :hook
  (company-mode . company-box-mode))

(provide 'completion-company)

;;; completion-company.el ends here
