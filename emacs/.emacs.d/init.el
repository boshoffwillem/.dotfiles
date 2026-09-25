;;; init.el --- Personal Emacs configuration  -*- lexical-binding: t; -*-

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

;; No clutter next to edited files: `make-backup-files' covers foo~,
;; `create-lockfiles' covers .#foo, and `auto-save-default' covers #foo#.
;; `auto-save-list-file-prefix' nil also stops the auto-save-list/ session
;; files under ~/.emacs.d.
(setq read-process-output-max (* 1024 1024)
      create-lockfiles nil
      make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
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

;; xclip bridges the kill-ring to the system clipboard for terminal Emacs.
;; Windows Emacs already talks to the native clipboard, and xclip's autodetection
;; falls back to a helper program that isn't installed there, so skip it.
(use-package xclip
  :straight t
  :if (not (memq system-type '(windows-nt ms-dos)))
  :config
  (when (executable-find xclip-program)
    (xclip-mode 1)))

;; Git for Windows ships GNU `diff'/`patch'/etc. under usr/bin, a sibling of
;; the Git install root -- but only cmd/mingw64/bin tend to be on the PATH
;; Emacs inherits, not usr/bin. So anything that shells out to `diff' (e.g.
;; apheleia's patch-based formatting) fails with "Searching for program:
;; ... diff" even though git itself works fine. Walk up from wherever
;; git.exe actually resolved from (cmd/ is 1 level under the Git root,
;; mingw64/bin/ is 2) rather than assuming a fixed depth. Append (not
;; prepend) to `exec-path' so this only fills in tools missing elsewhere,
;; rather than shadowing anything already found earlier on the path (e.g.
;; Windows' own find.exe).
(when (eq system-type 'windows-nt)
  (when-let* ((git (executable-find "git")))
    (let ((dir (file-name-directory git))
          (git-usr-bin nil)
          (depth 0))
      (while (and dir (not git-usr-bin) (< depth 4))
        (let ((candidate (expand-file-name "usr/bin" dir)))
          (if (file-directory-p candidate)
              (setq git-usr-bin candidate)
            (setq dir (file-name-directory (directory-file-name dir))
                  depth (1+ depth)))))
      (when git-usr-bin
        (add-to-list 'exec-path git-usr-bin t)))))

;; Emacs.app launched from the Dock/Finder inherits launchd's minimal PATH,
;; not the shell's, so anything only added to PATH in ~/.zshrc (flutter/dart,
;; cargo's rust-analyzer, /opt/homebrew/bin's kotlin-lsp, ...) is invisible
;; to it -- lsp-mode then finds no usable server and reports whichever
;; add-on client is left instead (e.g. "Command \"semgrep lsp\" is not
;; present on the path" when opening a .dart file). Copy PATH over from an
;; interactive login zsh; "-i" is needed since flutter's PATH entry lives in
;; ~/.zshrc, which a login-only shell doesn't read.
(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns))
  :custom
  (exec-path-from-shell-arguments '("-l" "-i"))
  :config
  (exec-path-from-shell-initialize))

;; (setq large-file-warning-threshold nil)
(global-auto-revert-mode t)
(setq auto-revert-interval 2)
(setq auto-revert-check-vc-info t)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose t)
(global-display-line-numbers-mode 1)

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
  (completion-styles '(orderless basic substring flex))
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
  ;; (doom-themes-treemacs-theme "doom-tokyo-night") ; use "doom-colors" for less minimal icon theme
  :config
  ;; (load-theme 'doom-tokyo-night t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package kanagawa-themes
  :straight t
  :config
  (load-theme 'kanagawa-dragon t))

(use-package nerd-icons
  :straight (nerd-icons
	     :type git
	     :host github
	     :repo "rainstormstudio/nerd-icons.el"
	     :files (:defaults "data"))
  :custom
  ;; "Ioskeley Mono" (our editor font) isn't Nerd-Font-patched, so icon
  ;; glyphs render as tofu in it. "Symbols Nerd Font Mono" is the glyph
  ;; font `nerd-icons-install-fonts' installs and is already present.
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

;; Dired file icons.
(use-package nerd-icons-dired
  :straight t
  :hook (dired-mode . nerd-icons-dired-mode))

;; Completion-candidate icons (find-file, switch-buffer, etc. via Vertico +
;; Marginalia annotations). `marginalia-mode' is already turned on by the
;; time this loads, so `:hook' on it would never fire -- load eagerly and
;; enable directly instead, and add the hook only to keep it in sync with
;; any future marginalia-mode toggling.
(use-package nerd-icons-completion
  :straight t
  :after marginalia
  :demand t
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

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
  (define-key evil-normal-state-map (kbd "<SPC>ps") 'project-find-regexp)
  (define-key evil-normal-state-map (kbd "<SPC>pp") 'project-switch-project)
  (define-key evil-normal-state-map (kbd "<SPC>pb") 'project-switch-to-buffer)
  (define-key evil-normal-state-map (kbd "<SPC>:") 'execute-extended-command)
  (define-key evil-normal-state-map (kbd "K") 'eldoc)
  
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
;; Evil Collection binds j/k in its own mode maps, and those take precedence
;; over `evil-normal-state-map', so the jkl; remap above never reaches dired or
;; magit.  Reapply it per mode once Evil Collection has finished its setup.
(defun my/evil-collection-jkl-remap (mode &rest _rest)
  "Apply the jkl; movement scheme to MODE after Evil Collection sets it up."
  (pcase mode
    ;; ";" is left alone in dired: Evil Collection uses it as the epa-dired
    ;; prefix (";e" encrypt, ";d" decrypt, ";s" sign, ";v" verify).
    ('dired
     (evil-collection-define-key 'normal 'dired-mode-map
       "j" 'evil-backward-char
       "k" 'dired-next-line
       "l" 'dired-previous-line))
    ;; Taking "l" for movement displaces magit's log popup, so shift it -- and
    ;; the `magit-log-refresh' it lands on -- one key along.
    ('magit
     (dolist (state '(normal visual))
       (evil-collection-define-key state 'magit-mode-map
         "j" 'evil-backward-char
         "k" 'evil-next-line
         "l" 'evil-previous-line
         ";" 'evil-forward-char
         "L" 'magit-log
         "\C-l" 'magit-log-refresh)))))

(use-package evil-collection
  :straight t
  :after evil
  :init
  (add-hook 'evil-collection-setup-hook #'my/evil-collection-jkl-remap)
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

(use-package xterm-color
  :straight t)

(use-package eshell
  :hook
  (eshell-before-prompt . (lambda ()
			    (setq xterm-color-preserve-properties t)))
  :config
  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))
  (setenv "TERM" "xterm-256color")
  )

;; (use-package projectile
;;   :straight t
;;   :config
;;   (projectile-mode +1)
;;   (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;   )

(global-set-key (kbd "C-c psd") #'project-switch-project)

(use-package deadgrep
  :straight t
  :config
  (global-set-key (kbd "C-c psd") #'deadgrep)
  )

;; Always show deadgrep and project-find-regexp (xref) results in a bottom
;; horizontal window, rather than wherever `display-buffer' would otherwise
;; put them (e.g. a vertical split next to the current window).
(add-to-list 'display-buffer-alist
             '((or (major-mode . deadgrep-mode)
                   (major-mode . xref--xref-buffer-mode))
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (side . bottom)
               (slot . 0)
               (window-height . 0.35)
               (dedicated . t)))

(use-package magit
  :straight t
  :config
  (global-set-key (kbd "C-c g") #'magit-status)
  )

;; `treemacs-position' defaults to `left', so no extra config needed for the
;; window side.
(use-package treemacs
  :straight t
  :init
  (global-set-key (kbd "C-c t") #'my/treemacs-toggle-current-project)
  :config
  (defun my/treemacs-toggle-current-project ()
    "Toggle treemacs, always showing just the current buffer's project
\(git repo root, as found by `project.el') instead of a manually
maintained workspace of projects."
    (interactive)
    (if (eq (treemacs-current-visibility) 'visible)
        (delete-window (treemacs-get-local-window))
      (treemacs-add-and-display-current-project-exclusively))))

;; Evil integration for treemacs -- gives treemacs buffers their own evil
;; state (`treemacs-mode' starts in it) with its own keymap, so the jkl;
;; remap from `evil-normal-state-map' above doesn't reach it; reapply the
;; same j/k/l/; rotation (back/down/up/forward) here.
(use-package treemacs-evil
  :straight t
  :after (treemacs evil)
  :config
  ;; `;' should only ever expand a directory, never collapse one, so make
  ;; RET-action a no-op on already-open dir/root nodes instead of the
  ;; default toggle (closed dir/root nodes still expand via toggle).
  (treemacs-define-RET-action 'dir-node-open #'ignore)
  (treemacs-define-RET-action 'root-node-open #'ignore)
  (evil-define-key 'treemacs treemacs-mode-map
    (kbd "j") #'treemacs-COLLAPSE-action
    (kbd "k") #'treemacs-next-line
    (kbd "l") #'treemacs-previous-line
    (kbd ";") #'treemacs-RET-action))

;; Nerd Font file/directory icons in the treemacs sidebar.
;; (`doom-themes-treemacs-config' above is a no-op stub in the currently
;; installed doom-themes version, so it doesn't conflict with this.)
(use-package treemacs-nerd-icons
  :straight t
  :after treemacs
  :config
  (treemacs-nerd-icons-config))

(use-package company
  :straight t
  :config
  (setq company-idle-delay 0.0
	company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode)
  )

(use-package flycheck
  :straight t
  :ensure t
  :custom
  ;; *scratch* starts in `lisp-interaction-mode', which has no checker --
  ;; skip it so startup doesn't complain "no syntax checker ... can run here".
  (flycheck-global-modes '(not lisp-interaction-mode))
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (define-key evil-normal-state-map (kbd "<SPC>e") 'flycheck-explain-error-at-point)
  )

(load (locate-user-emacs-file "frontend"))
(load (locate-user-emacs-file "vue"))
(load (locate-user-emacs-file "react"))
(load (locate-user-emacs-file "csharp"))
(load (locate-user-emacs-file "kotlin"))
(load (locate-user-emacs-file "swift"))
(load (locate-user-emacs-file "dart"))
(load (locate-user-emacs-file "rust"))
(load (locate-user-emacs-file "zig"))
(load (locate-user-emacs-file "yaml"))
(load (locate-user-emacs-file "markdown"))

;; (use-package fsharp-mode
;;   :straight t)

;; (use-package feature-mode
;;   :straight t)

;; (use-package elixir-mode
;;   :straight t
;;   :ensure t)

;;npm install -g pyright
;; sudo apt install python3-pylsp python3-pylsp-isort python3-pylsp-black -y

;; (use-package python-black
;;   :straight t
;;   :ensure t
;;   :demand t
;;   :after python
;;   :hook ((python-mode . python-black-on-save-mode)))

;; (use-package pyvenv
;;   :straight t
;;   :ensure t
;;   :config
;;   (pyvenv-mode t)

;; Set correct Python interpreter
;; (setq pyvenv-post-activate-hooks
;;       (list (lambda ()
;;               (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
;; (setq pyvenv-post-deactivate-hooks
;;       (list (lambda ()
;;               (setq python-shell-interpreter "python3"))))
;; )

(use-package apheleia
  :straight t
  :config
  (apheleia-global-mode +1))

(use-package ace-window
  :straight t
  :config
  (global-set-key (kbd "C-c o") 'ace-window)
  )

;; (use-package lsp-treemacs
;;   :straight t)

;; (use-package dap-mode
;;   :straight t
;;   )

;;; init.el ends here
