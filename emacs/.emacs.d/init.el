(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(straight-use-package 'use-package)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      create-lockfiles nil
      make-backup-files nil
      )

(defun dired-up-directory-same-buffer ()
  "Go up in the same buffer."
  (find-alternate-file ".."))
(defun my-dired-mode-hook ()
  (put 'dired-find-alternate-file 'disabled nil) ; Disables the warning.
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "^") 'dired-up-directory-same-buffer))
(add-hook 'dired-mode-hook #'my-dired-mode-hook)
(setq dired-use-ls-dired nil)

(use-package drag-stuff
  :straight t
  :bind
  (("M-l" . drag-stuff-up)
   ("M-k" . drag-stuff-down))
  :config
  (drag-stuff-global-mode 1))

;; (setq large-file-warning-threshold nil)
;; (global-auto-revert-mode t)
;; (setq auto-revert-interval 2)
;; (setq auto-revert-check-vc-info t)
;; (setq global-auto-revert-non-file-buffers t)
;; (setq auto-revert-verbose t)

;; Enable Vertico.
(use-package vertico
  :straight t
  ;;:custom
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

;; Optionally use the `orderless' completion style.
(use-package orderless
  :straight t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic substring))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :straight t
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

(use-package doom-themes
  :ensure t
  :straight t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; for treemacs users
  (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  :config
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(add-to-list 'default-frame-alist
             '(font . "Ioskeley Mono-11"))

(use-package nerd-icons
  :straight (nerd-icons
	     :type git
	     :host github
	     :repo "rainstormstudio/nerd-icons.el"
	     :files (:defaults "data"))
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Ioskeley Mono")
    )

(which-key-mode)

(use-package evil
  :straight t
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
  (define-key evil-normal-state-map (kbd "<SPC>bk") 'kill-buffer)
  (define-key evil-normal-state-map (kbd "<SPC>sf") 'find-file)
  (define-key evil-normal-state-map (kbd "<SPC>s.") 'recentf)
  (define-key evil-normal-state-map (kbd "<SPC>pf") 'project-find-file)
  (define-key evil-normal-state-map (kbd "<SPC>ps") 'project-search)
  (define-key evil-normal-state-map (kbd "<SPC>pp") 'project-switch-project)
  (define-key evil-normal-state-map (kbd "<SPC>pb") 'project-switch-to-buffer)
  
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
  :straight t
  :after evil
  :config
  (evil-collection-init))

;; Evil Commentary - Easy commenting (gc operator)
(use-package evil-commentary
  :straight t
  :after evil
  :config
  (evil-commentary-mode))

;; Evil Surround - Surround text objects (like vim-surround)
(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package magit
  :straight t
  )

(use-package company
  :straight t
  :config
  (setq company-idle-delay 0.0
	company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode)
  )

;; (use-package corfu
;;   :straight t
;;   :custom
;;   (corfu-auto nil)         ; Manual completion trigger
;;   (corfu-cycle t)          ; Cycle through candidates
;;   (corfu-preselect 'first))

(use-package flycheck
  :straight t
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package yasnippet
  :straight t
  )

;;dotnet tool install --global csharp-ls

(use-package web-mode
  :straight t
  :ensure t
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.cshtml?\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)))

(add-to-list 'auto-mode-alist '("\\.csproj\\'" . nxml-mode))

(require 'eglot)
(setq eglot-server-programs
      '((csharp-mode . ("csharp-ls"))
	(csharp-ts-mode . ("csharp-ls"))
	))

(add-hook 'prog-mode-hook 'eglot-ensure)

(setq eglot-ignored-server-capabilities
      '(
        ;; :hoverProvider                    ; Documentation on hover
        ;; :completionProvider               ; Code completion
        ;; :signatureHelpProvider            ; Function signature help
        ;; :definitionProvider               ; Go to definition
        ;; :typeDefinitionProvider           ; Go to type definition
        ;; :implementationProvider           ; Go to implementation
        ;; :declarationProvider              ; Go to declaration
        ;; :referencesProvider               ; Find references
        ;; :documentHighlightProvider        ; Highlight symbols automatically
        ;; :documentSymbolProvider           ; List symbols in buffer
        ;; :workspaceSymbolProvider          ; List symbols in workspace
        ;; :codeActionProvider               ; Execute code actions
        ;; :codeLensProvider                 ; Code lens
        ;; :documentFormattingProvider       ; Format buffer
        ;; :documentRangeFormattingProvider  ; Format portion of buffer
        ;; :documentOnTypeFormattingProvider ; On-type formatting
        ;; :renameProvider                   ; Rename symbol
        ;; :documentLinkProvider             ; Highlight links in document
        ;; :colorProvider                    ; Decorate color references
        ;; :foldingRangeProvider             ; Fold regions of buffer
        ;; :executeCommandProvider           ; Execute custom commands
        ;; :inlayHintProvider                ; Inlay hints
        ))

;; (use-package lsp-mode
;;   :straight t
;;   :config
;;   (define-key evil-normal-state-map (kbd "gd") 'lsp-goto-type-definition)
;;   (define-key evil-normal-state-map (kbd "gi") 'lsp-goto-implementation)
;;   :hook
;;   (
;;    (csharp-mode . lsp)
;;    (csharp-ts-mode .lsp)
;;    (web-mode .lsp)
;;    )
;;   )

;; (use-package dap-mode
;;   :straight t
;;   )

;; (add-hook 'prog-mode-hook #'lsp)

;; (with-eval-after-load 'lsp-mode
;;   (require 'dap-chrome)
;;   (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
;;   (yas-global-mode))
