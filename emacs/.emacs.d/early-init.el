;;; early-init.el --- Personal Emacs configuration  -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; Raise the GC threshold before package/config loading begins, so it's in
;; effect for the whole startup, not just from init.el onward.
(setq gc-cons-threshold (* 100 1024 1024))

;; Native-compiling third-party packages in the background emits byte-compiler
;; warnings that are only about how the package's source is written, not
;; real runtime problems (e.g. lsp-treemacs calling
;; `treemacs-define-doubleclick-action' without requiring the treemacs file
;; that defines it). Keep logging them to *Warnings*, but stop them from
;; popping that buffer up mid-edit.
(setq native-comp-async-report-warnings-errors 'silent)

;; Disable the tool bar and set the default font before the first frame is
;; created, to avoid a visible flash of the tool bar / default font.
(tool-bar-mode -1)
(add-to-list 'default-frame-alist
             '(font . "Ioskeley Mono-10"))

;;; early-init.el ends here
