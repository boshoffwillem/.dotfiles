;;; ~/.emacs.d/init.el --- Summary
;;; Commentary: Complete Emacs configuration for C# development with Evil mode and Gruvbox theme

;; Package management setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)
(cl-dolist (mode '(emacs-lisp-mode lisp-interaction-mode))
  (font-lock-add-keywords
   mode
   '(("(\\<\\(straight-use-package\\)\\>" 1 font-lock-keyword-face))))
(setq straight-use-package-by-default 1)

;; Suppress native compilation warnings
(setq native-comp-async-report-warnings-errors nil)
(setq warning-minimum-level :emergency)

;; Disable audio feedback
(setq ring-bell-function 'ignore)

;; Hide UI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell nil)

(use-package autothemer :ensure t)
(use-package rose-pine-emacs)
;;(load-theme 'rose-pine-color t)

;; Evil Mode - Vim emulation
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-i-jump nil
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  
  ;; Remap movement keys: j k l ; instead of h j k l
  ;; Normal state
  (define-key evil-normal-state-map (kbd "j") 'evil-backward-char)
  (define-key evil-normal-state-map (kbd "k") 'evil-next-line)
  (define-key evil-normal-state-map (kbd "l") 'evil-previous-line)
  (define-key evil-normal-state-map (kbd ";") 'evil-forward-char)
  (define-key evil-normal-state-map (kbd "<SPC><SPC>") 'switch-to-buffer)
  
  ;; Visual state
  (define-key evil-visual-state-map (kbd "j") 'evil-backward-char)
  (define-key evil-visual-state-map (kbd "k") 'evil-next-line)
  (define-key evil-visual-state-map (kbd "l") 'evil-previous-line)
  (define-key evil-visual-state-map (kbd ";") 'evil-forward-char)
  
  ;; Motion state (used by operators)
  (define-key evil-motion-state-map (kbd "j") 'evil-backward-char)
  (define-key evil-motion-state-map (kbd "k") 'evil-next-line)
  (define-key evil-motion-state-map (kbd "l") 'evil-previous-line)
  (define-key evil-motion-state-map (kbd ";") 'evil-forward-char)
  
  ;; Remap the displaced keys to their original functions
  (define-key evil-normal-state-map (kbd "h") 'evil-join)
  (define-key evil-visual-state-map (kbd "h") 'evil-join)
  
  ;; Use Emacs state in some modes
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Evil Collection - Evil bindings for many modes
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Evil Commentary - Easy commenting (gc operator)
(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

;; Evil Surround - Surround text objects (like vim-surround)
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; Enable Vertico.
(use-package vertico
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)
  (setq completion-styles '(basic substring partial-completion flex))
  ) ;; Emacs 31: partial-completion behaves like substring

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; LSP Mode - Core language server support
(use-package lsp-mode
  :hook ((csharp-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-keymap-prefix "C-c l"
	lsp-auto-guess-root t
	lsp-log-io nil
	lsp-restart 'auto-restart
	lsp-enable-snippet t
	lsp-prefer-flymake nil
	lsp-lens-place-position 'above-line
	;; File watching settings
	lsp-enable-file-watchers nil
	;; lsp-file-watch-threshold 5000  ; Increase from default 1000
	;; lsp-file-watch-ignored-directories
	;; '("[/\\\\]\\.git$"
	;;   "[/\\\\]\\.github$"
	;;   "[/\\\\]\\.circleci$"
	;;   "[/\\\\]\\.hg$"
	;;   "[/\\\\]\\.bzr$"
	;;   "[/\\\\]_darcs$"
	;;   "[/\\\\]\\.svn$"
	;;   "[/\\\\]FOSSIL$"
	;;   "[/\\\\]\\.idea$"
	;;   "[/\\\\]\\.ensime_cache$"
	;;   "[/\\\\]\\.eunit$"
	;;   "[/\\\\]node_modules"
	;;   "[/\\\\]\\.yarn$"
	;;   "[/\\\\]\\.fslckout$"
	;;   "[/\\\\]\\.tox$"
	;;   "[/\\\\]dist$"
	;;   "[/\\\\]dist-newstyle$"
	;;   "[/\\\\]\\.stack-work$"
	;;   "[/\\\\]\\.bloop$"
	;;   "[/\\\\]\\.metals$"
	;;   "[/\\\\]target$"
	;;   "[/\\\\]\\.ccls-cache$"
	;;   "[/\\\\]\\.vscode$"
	;;   "[/\\\\]\\.venv$"
	;;   "[/\\\\]\\.mypy_cache$"
	;;   "[/\\\\]\\.pytest_cache$"
	;;   "[/\\\\]pycache$"
	;;   "[/\\\\]bazel-[^/\\\\]+$"
	;;   "[/\\\\]\\.cask$"
	;;   "[/\\\\]bin/Debug$"
	;;   "[/\\\\]bin/Release$"
	;;   "[/\\\\]obj$"
	;;   "[/\\\\]packages$"
	;;   "[/\\\\]\\.vs$")  ;; Evil-friendly LSP keybindings
	)
  (evil-define-key 'normal lsp-mode-map
    (kbd "gd") 'lsp-find-definition
    (kbd "gr") 'lsp-find-references
    (kbd "gi") 'lsp-find-implementation
    (kbd "K") 'lsp-describe-thing-at-point))

;; LSP UI - Enhanced UI features
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t))

;; Company - Autocompletion
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-selection-wrap-around t)
  ;; Evil-friendly company navigation
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous))

;; Flycheck - Syntax checking
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  ;; Evil keybindings for navigating errors
  (evil-define-key 'normal 'global
    (kbd "] e") 'flycheck-next-error
    (kbd "[ e") 'flycheck-previous-error))

;; C# Mode
(use-package csharp-mode
  :mode "\\.cs\\'"
  :config
  (add-hook 'csharp-mode-hook
            (lambda ()
              (setq c-basic-offset 4
                    tab-width 4
                    indent-tabs-mode nil))))

;; Projectile - Project management
(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  (setq projectile-project-search-path '("~/code/")
        projectile-enable-caching t))

;; Which-key - Show available keybindings
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

;; Treemacs - File explorer
(use-package treemacs
  :bind ("C-c t" . treemacs)
  :config
  (use-package treemacs-evil
    :after (treemacs evil)))

;; Magit - Git integration
(use-package magit
  :bind ("C-x g" . magit-status))

;; Snippets for common C# patterns
(use-package yasnippet
  :init (yas-global-mode 1))

(use-package yasnippet-snippets)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
