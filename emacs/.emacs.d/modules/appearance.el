;;; appearance.el --- appearance for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

(use-package diminish)
(use-package highlight-indent-guides)
(setq highlight-indent-guides-method 'bitmap)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(use-package doom-themes
  :config
  ;; (load-theme 'doom-dark+)
  ;; (load-theme 'doom-one)
  ;; (load-theme 'modus-vivendi)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package autothemer)

(use-package color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-night)

(use-package ample-theme)

(use-package afternoon-theme)

(straight-use-package
 '(rose-pine-emacs
   :host github
   :repo "thongpv87/rose-pine-emacs"
   :branch "master"))
;; (load-theme 'rose-pine-moon t)

;; (use-package doom-modeline
;;   :init
;;   (doom-modeline-mode 1))

;; (use-package treemacs)

(use-package all-the-icons)

(use-package all-the-icons-dired
  :after all-the-icons
  :hook
  ((dired-mode . all-the-icons-dired-mode))
  )

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package dashboard
  ;; :after evil
  :init
  (progn
    (setq dashboard-items '((recents . 5)
                            ;; (projects . 5)
                            (bookmarks . 5)
                            (agenda . 5)))
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-heading-icons t)
    )
  :config
  (dashboard-setup-startup-hook)
  )

(use-package ligature
  :load-path "path-to-ligature-repo"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;; (use-package unicode-fonts
;;    :config
;;     (unicode-fonts-setup))

(global-display-line-numbers-mode)

(provide 'appearance)

;;; appearance.el ends here
