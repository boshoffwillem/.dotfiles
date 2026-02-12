;;; project-management.el --- project management for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:
;; This file contains project management configuration

;;; Code:
(use-package projectile
  :config
  (projectile-mode +1)
  ;; Recommended keymap prefix on macOS
  ;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  ;; Recommended keymap prefix on Windows/Linux
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(provide 'project-management)

;;; project-management.el ends here
