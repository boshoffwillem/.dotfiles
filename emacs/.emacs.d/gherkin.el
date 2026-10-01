;;; gherkin.el --- Gherkin (.feature) file support  -*- lexical-binding: t; -*-

;; `feature-mode' (the classic cucumber.el major mode) provides syntax
;; highlighting and indenting for Gherkin: Feature/Scenario/Given/When/
;; Then/And/But/Background keywords, PyString blocks, and tags.  It
;; registers an `auto-mode-alist' entry for `\.feature' via its own
;; autoloads, and also activates inside Ruby/Cucumber step files.
;;
;; Handy keys (from the package):
;;   C-c , v   run cucumber for the current feature

(use-package feature-mode
  :straight t
  :mode ("\\.feature\\'" . feature-mode))

(provide 'gherkin)

;;; gherkin.el ends here