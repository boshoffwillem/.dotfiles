;;; ide.el --- ide configuration for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:

;; ==================================== Project wide searching using ripgrep
(use-package deadgrep)

;; ==================================== Search and replace with regular expressions
(use-package visual-regexp)

(use-package flycheck
  :custom
  (flycheck-display-errors-delay 0.1)
  :config
  (global-flycheck-mode)
  )

(use-package yasnippet
  :after company
  :config
  (yas-reload-all)
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :after yasnippet)

;; (use-package dap-mode
;;   :commands (dap-debug dap-breakpoints-add)
;;   :init
;;   (dap-mode 1)
;;   (dap-ui-mode 1)
;;   (dap-auto-configure-mode)
;;   (require 'dap-netcore)
;;   :custom
;;   (dap-netcore-install-dir "/home/hoagie/.emacs.d/.cache/"))

;; (use-package posframe)

;; (defun dap-netcore--populate-default-args (conf)
;;   "Populate CONF with the default arguments."
;;   (dap--put-if-absent conf :cwd default-directory)
;;   (dap--put-if-absent conf :program (read-file-name "Select an executable:" (concat (lsp-workspace-root) "bin/Debug")))
;;   (dap--put-if-absent conf :dap-server-path (list (dap-netcore--debugger-locate) "--interpreter=vscode")))

;; (dap-register-debug-provider
;;  "Psicle SERVER_DEBUG"
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

;; .editorconfig files
(use-package editorconfig
  :config
  (editorconfig-mode)
  )

;; (use-package feature-mode)

;; (use-package restclient)

;; .xml files
;; (setq nxml-slash-auto-complete-flag t)
;; (setq nxml-child-indent 4)
;; (setq nxml-attribute-indent 4)
;; (add-to-list 'auto-mode-alist '("\\.csproj\\'" . nxml-mode))
;; (add-to-list 'auto-mode-alist '("\\.nuspec\\'" . nxml-mode))
;; (add-to-list 'auto-mode-alist '("\\.xaml\\'" . nxml-mode))

;; LSP
(use-package lsp-mode
  :bind
  (:map lsp-mode-map
        ("C-c l t r" . lsp-csharp-run-test-at-point)
        ("C-c l r a" . lsp-csharp-run-all-tests-in-buffer)
        )
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-lens-place-position 'above-line)
  :custom
  (setq lsp-idle-delay 0.5)
  (setq lsp-log-io nil)
  (setq lsp-auto-execute-action nil)
  (setq lsp-enable-file-watchers nil)
  (setq lsp-lens-enable t)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (setq lsp-modeline-code-actions-enable t)
  (setq lsp-modeline-diagnostics-enable t)
  (setq lsp-modeline-diagnostics-scope :workspace)
  (lsp-eldoc-render-all t)
  ;; (lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; (lsp-rust-analyzer-server-display-inlay-hints t)
  ;; (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  ;; (lsp-rust-analyzer-display-chaining-hints t)
  ;; (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  ;; (lsp-rust-analyzer-display-closure-return-type-hints t)
  ;; (lsp-rust-analyzer-display-parameter-hints nil)
  ;; (lsp-rust-analyzer-display-reborrow-hints nil)
  :hook (
         (lsp-mode . lsp-enable-which-key-integration)
         (csharp-mode . lsp-deferred)
         (yaml-ts-mode . lsp-deferred)
         (lsp-mode . (lambda ()
                       (setq-local company-backends '(company-capf :with (company-tabnine))))))
  :commands (lsp lsp-deferred))

;; tree-sitter
;; (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
;; (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;; (hcl "https://github.com/mitchellh/tree-sitter-hcl")
;; (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;; (proto "Https://github.com/mitchellh/tree-sitter-proto")

;; (use-package terraform-mode
  ;; :hook (terraform-mode . lsp-deferred))

;; (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-ts-mode))
;; (add-hook 'csharp-ts-mode-hook #'lsp-deferred)
;; (add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-ts-mode))
;; (add-hook 'dockerfile-ts-mode-hook #'lsp-deferred)
;; (add-to-list 'auto-mode-alist '("\\.tf\\'" . hcl-ts-mode))
;; (add-hook 'hcl-ts-mode-hook #'lsp-deferred)
;; (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
;; (add-hook 'json-ts-mode-hook #'lsp-deferred)
;; (add-to-list 'auto-mode-alist '("\\.proto\\'" . proto-ts-mode))
;; (add-hook 'proto-ts-mode-hook #'lsp-deferred)
;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
;; (add-hook 'rust-ts-mode-hook #'lsp-deferred)

;; (yaml "https://github.com/ikatyang/tree-sitter-yaml")
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))

(use-package lsp-treemacs
  :config
  (lsp-treemacs-sync-mode 1)
  :commands lsp-treemacs-errors-list)

(use-package consult-lsp
  :config
  ;; find symbol in project.
  ;; (define-key lsp-mode-map (kbd "C-c p t") 'consult-lsp-symbols)
  ;; (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
  )

;; (add-to-list 'image-types 'svg)
(provide 'ide)

;;; ide.el ends here
