;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Willem Boshoff"
      user-mail-address "boshoffwillem@protonmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Noto Sans Mono" :size 12 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Cantarell" :size 12))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dark+)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Improve garbage collection performance.
;; (setq gc-cons-threshold (* 100 1024 1024))

;; Improve processing of sub-processes that generates large chunk.
;; (setq read-process-output-max (* 1024 1024))

;; Set default coding system (especially for Windows)
;; (prefer-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-language-environment 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (setq large-file-warning-threshold 100000000) ;; change to ~100 MB
;; (setq org-src-preserve-indentation t)
;;
;; (setq make-backup-files nil
;;       auto-save-default nil
;;       create-lockfiles nil)
;;
;; (setq custom-safe-themes t)
;;
;; (defun dired-up-directory-same-buffer ()
;;   "Go up in the same buffer."
;;   (find-alternate-file ".."))
;; (defun my-dired-mode-hook ()
;;   (put 'dired-find-alternate-file 'disabled nil) ; Disables the warning.
;;   (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
;;   (define-key dired-mode-map (kbd "^") 'dired-up-directory-same-buffer))
;; (add-hook 'dired-mode-hook #'my-dired-mode-hook)
;; (setq dired-use-ls-dired nil)

;; (setq default-directory "~/code/")
;; (setq large-file-warning-threshold nil)
;; Set default bookmarks directory.
;; (setq bookmark-default-file "~/emacs-files/bookmarks")
;; Delete selected text instead of inserting.
;; (delete-selection-mode 1)
;; Emacs has problems with very long lines. so-long detects them and takes appropriate action.
;; Good for minified code and whatnot.
;; (global-so-long-mode)
;; I want recent files
;; (require 'recentf)
;; (recentf-mode)

;; (global-auto-revert-mode t)
;; (setq auto-revert-interval 2)
;; (setq auto-revert-check-vc-info t)
;; (setq global-auto-revert-non-file-buffers t)
;; (setq auto-revert-verbose t)
;;
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(evil-define-key '(normal visual) 'global (kbd "j") 'evil-backward-char)
(evil-define-key '(normal visual) 'global (kbd "k") 'evil-next-line)
(evil-define-key '(normal visual) 'global (kbd "l") 'evil-previous-line)
(evil-define-key '(normal visual) 'global (kbd ";") 'evil-forward-char)
(evil-define-key '(normal visual) 'global (kbd "gd") 'lsp-find-definition)
(evil-define-key '(normal visual) 'global (kbd "gi") 'lsp-find-implementation)
(evil-define-key '(normal visual) 'global (kbd "gr") 'lsp-find-references)

;; mapping to leader
;; (map! :leader
;;       "x" #'your-command-here
;;       "y" (cmd! (message "Hello world"))
;;       "f" nil) ; This unbinds what was previously bound to <leader> f
;; - :leader specifies that the binding should be under the leader key.
;; - "x" is the key sequence you want to bind after the leader key (e.g., SPC x).
;; - #'your-command-here is the function or command you want to execute when the key sequence is pressed.
;; - You can also use (cmd! ...) to define an anonymous command directly within the map! macro.
;; - Setting a key to nil unbinds any existing binding for that key sequence.

(map! :leader
      "k" #'kill-buffer)

(map! :leader
      "<SPC>" #'consult-buffer)

;; (unless (display-graphic-p)
;;   (require 'evil-terminal-cursor-changer)
;;   (evil-terminal-cursor-changer-activate) ; or (etcc-on)
;;   )

(use-package! lsp-mode
  :custom
  (setq lsp-enable-file-watchers nil)
  )

;; (use-package! lsp-mode
;;   :bind
;;   (:map lsp-mode-map
;;         ("C-c l t r" . lsp-csharp-run-test-at-point)
;;         ("C-c l r a" . lsp-csharp-run-all-tests-in-buffer)
;;         )
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :config
;;   (setq lsp-lens-place-position 'above-line)
;;   :custom
;;   (setq lsp-idle-delay 0.5)
;;   (setq lsp-log-io nil)
;;   (setq lsp-auto-execute-action nil)
;;   (setq lsp-lens-enable t)
;;   (setq lsp-headerline-breadcrumb-enable nil)
;;   (setq lsp-headerline-breadcrumb-enable-symbol-numbers nil)
;;   (setq lsp-modeline-code-actions-enable t)
;;   (setq lsp-modeline-diagnostics-enable t)
;;   (setq lsp-modeline-diagnostics-scope :workspace)
;;   (lsp-eldoc-render-all t)
;;   :hook (
;;          (lsp-mode . lsp-enable-which-key-integration)
;;          (terraform-mode . lsp-deferred)
;;          )
;;   :commands (lsp lsp-deferred)
;;   )

;; (use-package! xclip
;;   :config
;;   (xclip-mode))
;;
;; (use-package! company
;;   :config
;;   ;; Trigger completion immediately.
;;   (setq company-idle-delay 0)
;;   ;; Number the candidates (use M-1, M-2 etc to select completions).
;;   (setq company-show-quick-access t))

;; (after! company
;;   (setq +lsp-company-backends '(company-tabnine :separate company-capf company-yasnippet))
;;   )
;; (use-package! tabnine
;;   :config
;;   (tabnine-mode))
;;
;; (use-package! drag-stuff
;;   :bind
;;   (("M-l" . drag-stuff-up)
;;    ("M-k" . drag-stuff-down))
;;   :config
;;   (drag-stuff-global-mode 1))
