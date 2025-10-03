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

;; OmniSharp auto-installer
(defun setup-omnisharp ()
  "Check if OmniSharp is installed, and install it if not."
  (interactive)
  (let* ((omnisharp-dir (expand-file-name "~/.local/bin"))
         (omnisharp-path (concat omnisharp-dir "/omnisharp"))
         (download-url "https://github.com/OmniSharp/omnisharp-roslyn/releases/latest/download/omnisharp-linux-x64.tar.gz")
         (tar-file (concat omnisharp-dir "/omnisharp-linux-x64.tar.gz")))
    
    (if (file-exists-p omnisharp-path)
        (message "OmniSharp is already installed at %s" omnisharp-path)
      
      (progn
        (message "OmniSharp not found. Installing...")
        
        ;; Create directory if it doesn't exist
        (unless (file-exists-p omnisharp-dir)
          (make-directory omnisharp-dir t)
          (message "Created directory: %s" omnisharp-dir))
        
        ;; Download OmniSharp
        (message "Downloading OmniSharp from %s..." download-url)
        (url-copy-file download-url tar-file t)
        (message "Download complete.")
        
        ;; Extract the tarball
        (message "Extracting OmniSharp...")
        (let ((default-directory omnisharp-dir))
          (shell-command (format "tar xzf %s" tar-file)))
        
        ;; Make omnisharp executable
        (set-file-modes omnisharp-path #o755)
        
        ;; Clean up tar file
        (delete-file tar-file)
        
        (if (file-exists-p omnisharp-path)
            (message "OmniSharp installed successfully at %s" omnisharp-path)
          (error "OmniSharp installation failed. Please install manually."))))))

;; Optional: Run setup check on startup
(defun check-omnisharp-on-csharp-mode ()
  "Check for OmniSharp when opening a C# file."
  (let ((omnisharp-path (expand-file-name "~/.local/bin/omnisharp")))
    (unless (file-exists-p omnisharp-path)
      (when (yes-or-no-p "OmniSharp is not installed. Would you like to install it now? ")
        (setup-omnisharp)))))

;; Add to csharp-mode hook
(add-hook 'csharp-mode-hook #'check-omnisharp-on-csharp-mode)

;; Gruvbox Theme
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t))  ; Options: gruvbox-dark-hard, gruvbox-dark-medium, gruvbox-dark-soft, gruvbox-light-hard, gruvbox-light-medium, gruvbox-light-soft

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
        lsp-prefer-flymake nil)
  ;; Evil-friendly LSP keybindings
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
  (setq projectile-project-search-path '("~/projects/")
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

;; Better completion with Ivy/Counsel
(use-package ivy
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) ")
  ;; Evil-friendly ivy navigation
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)))

;; Snippets for common C# patterns
(use-package yasnippet
  :init (yas-global-mode 1))

(use-package yasnippet-snippets)

;; General.el - Better keybinding management
(use-package general
  :demand t
  :config
  ;; Set up leader key (space in normal mode)
  (general-create-definer my-leader-def
    :states '(normal visual insert emacs)
    :prefix "SPC"
    :non-normal-prefix "M-SPC")
  
  ;; Leader key mappings
  (my-leader-def
    "f" '(:ignore t :which-key "files")
    "ff" 'counsel-find-file
    "fr" 'counsel-recentf
    "fs" 'save-buffer
    
    "b" '(:ignore t :which-key "buffers")
    "bb" 'counsel-switch-buffer
    "bd" 'kill-this-buffer
    "bn" 'next-buffer
    "bp" 'previous-buffer
    
    "p" '(:ignore t :which-key "project")
    "pf" 'projectile-find-file
    "pp" 'projectile-switch-project
    "ps" 'projectile-grep
    
    "g" '(:ignore t :which-key "git")
    "gs" 'magit-status
    "gb" 'magit-blame
    
    "l" '(:ignore t :which-key "lsp")
    "la" 'lsp-execute-code-action
    "lr" 'lsp-rename
    "lf" 'lsp-format-buffer
    
    "w" '(:ignore t :which-key "window")
    "wh" 'evil-window-left
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wl" 'evil-window-right
    "ws" 'evil-window-split
    "wv" 'evil-window-vsplit
    "wd" 'evil-window-delete))

;; Custom settings
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
