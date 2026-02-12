;;; ide.el --- IDE setup for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:
;; This file contains project management configuration

;; ==================================== Project wide searching using ripgrep
(use-package deadgrep)

;; ==================================== Search and replace with regular expressions
(use-package visual-regexp)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error) ; optional but recommended error navigation
              ("M-p" . flycheck-previous-error)))

(use-package apheleia
  :diminish ""
  :config
  (apheleia-global-mode +1))
(push '(csharpier . ("dotnet"
                     "csharpier"
                     ))
      apheleia-formatters)
(push '(buf . ("buf"
               "format"
               filepath
               "-w"
               ))
      apheleia-formatters)
(push '(csharp-mode . csharpier)
      apheleia-mode-alist)
(push '(protobuf-mode . buf)
      apheleia-mode-alist)

;; .editorconfig files
(use-package editorconfig
  :config
  (editorconfig-mode)
  )

;; (use-package csharp-mode
;;   :config
;;   (require 'dap-netcore)
;;   :mode "\\.cs\\'"
;;   :hook
;;   (csharp-mode . lsp)
;;   )

(use-package feature-mode)

(use-package protobuf-mode)

(use-package powershell)

(use-package terraform-mode
  :mode "\\.tf\\'"
  :hook
  (terraform-mode . lsp)
  (terraform-mode . terraform-format-on-save-mode)
  )

(use-package kotlin-ts-mode
  :straight (:host gitlab :repo "bricka/emacs-kotlin-ts-mode")
  :mode "\\.kt\\'" ; if you want this mode to be auto-enabled
  :config
  (require 'dap-kotlin)
  (setq lsp-kotlin-debug-adapter-path (or (executable-find "kotlin-debug-adapter") ""))
  :hook
  (kotlin-ts-mode . lsp)
  )

(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

;; (use-package restclient)

;; .xml files
(setq nxml-slash-auto-complete-flag t)
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.nuspec\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xaml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.axaml\\'" . nxml-mode))

(defun os/setup-install-grammars ()
  "Install Tree-sitter grammars if they are absent."
  (interactive)
  (dolist (grammar
           '((css "https://github.com/tree-sitter/tree-sitter-css")
             (bash "https://github.com/tree-sitter/tree-sitter-bash")
             (c "https://github.com/tree-sitter/tree-sitter-c")
             (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
             (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
             (go "https://github.com/tree-sitter/tree-sitter-go")
             (hcl "https://github.com/tree-sitter-grammars/tree-sitter-hcl")
             (html "https://github.com/tree-sitter/tree-sitter-html")
             (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
             (json "https://github.com/tree-sitter/tree-sitter-json")
             (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
             ;; (make "https://github.com/alemuller/tree-sitter-make")
             ;; (markdown "https://github.com/ikatyang/tree-sitter-markdown")
             ;; (proto "https://github.com/mitchellh/tree-sitter-proto")
             (python "https://github.com/tree-sitter/tree-sitter-python")
             (rust "https://github.com/tree-sitter/tree-sitter-rust")
             (toml "https://github.com/tree-sitter/tree-sitter-toml")
             (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
             (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
             (yaml "https://github.com/ikatyang/tree-sitter-yaml")
             ))
    (setq treesit-language-source-alist grammar)

    ;; Only install `grammar' if we don't already have it
    ;; installed. However, if you want to *update* a grammar then
    ;; this obviously prevents that from happening.
    (unless (treesit-language-available-p (car grammar))
      (treesit-install-language-grammar (car grammar)))))

;; (os/setup-install-grammars)
;; (add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.html\\'" . html-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.sh\\'" . bash-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
;; (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))


(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(use-package direnv
  :bind (("C-c d d" . direnv-mode)
         ("C-c d a" . direnv-allow)))

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;; (use-package lsp-treemacs
;;   :after lsp-mode
;;   :config
;;   (lsp-treemacs-sync-mode 1)
;;   )

;; (use-package consult-lsp
;;   :after lsp-mode
;;   :config
;;   ;; find symbol in project.
;;   ;; (define-key lsp-mode-map (kbd "C-c p t") 'consult-lsp-symbols)
;;   ;; (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
;;   )

;; (use-package dap-mode
;;   :after lsp-mode
;;   ;; :commands (dap-debug dap-breakpoints-add)
;;   :init
;;   ;; (dap-mode 1)
;;   ;; (dap-ui-mode 1)
;;   (dap-auto-configure-mode)
;;   ;; :custom
;;   ;; (dap-netcore-install-dir "~/.config/emacs/.cache/")
;;   )

;; (use-package posframe)

;; (defun dap-netcore--populate-default-args (conf)
;;   "Populate CONF with the default arguments."
;;   (dap--put-if-absent conf :cwd lsp-workspace-root)
;;   (dap--put-if-absent conf :program (read-file-name "Select an executable:"))
;;   (dap--put-if-absent conf :dap-server-path (list (dap-netcore--debugger-locate) "--interpreter=vscode")))

;; (dap-register-debug-provider
;;  ".NET"
;;  'dap-netcore--populate-default-args)

;; (dap-register-debug-template ".Net Core Launch (Psicle SERVER_DEBUG)"
;;                              (list :type "coreclr"
;;                                    :request "launch"
;;                                    :name "NetCoreDbg::Launch"
;;                                    :stopAtEntry t))

;; (dap-register-debug-template ".Net Core Attach (Psicle SERVER_DEBUG)"
;;                              (list :type "coreclr"
;;                                    :request "attach"
;;                                    :program ""
;;                                    :processId "${command:pickProcess}"
;;                                    :name "NetCoreDbg::Launch"
;;                                    :stopAtEntry f))

(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-mode-map
	      ("<tab>". tab-indent-or-complete)
	      ("TAB". tab-indent-or-complete))
  (:map company-active-map
	      ("C-n". company-select-next)
	      ("C-p". company-select-previous)
	      ("M-<". company-select-first)
	      ("M->". company-select-last)))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(setq image-types '(svg png gif tiff jpeg xpm xbm pbm imagemagick))
(provide 'ide)

;;; ide.el ends here
