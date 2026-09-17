;;; frontend.el --- shared web-mode/template-file setup  -*- lexical-binding: t; -*-

;; Vue, React, and any future component/SFC format (Svelte, Astro, ...) each
;; pick whichever major mode actually fits their file format -- see vue.el
;; and react.el for the reasoning per framework.
;;
;; web-mode is the one piece of that genuinely shared across multiple file
;; types -- both the server-templating formats below and Vue's SFCs -- so it
;; is installed/configured once here. Framework-specific files layer their
;; own :mode entries and settings on top via `add-to-list'/hooks rather than
;; re-declaring the package.

(defconst my/frontend-indent-width 2
  "Indent width shared across frontend component-file modes (Vue, React, ...).")

(use-package web-mode
  :straight t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.cshtml?\\'" . web-mode)
   ("\\.html?\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)))

(provide 'frontend)
;;; frontend.el ends here
