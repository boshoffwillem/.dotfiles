;;; early-init.el --- Personal Emacs configuration  -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; Raise the GC threshold before package/config loading begins, so it's in
;; effect for the whole startup, not just from init.el onward.
(setq gc-cons-threshold (* 100 1024 1024))

;; Disable the tool bar and set the default font before the first frame is
;; created, to avoid a visible flash of the tool bar / default font.
(tool-bar-mode -1)
(add-to-list 'default-frame-alist
             '(font . "Ioskeley Mono-10"))

;;; early-init.el ends here
