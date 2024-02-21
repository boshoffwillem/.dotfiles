;;; ide.el --- ide configuration for emacs -*- lexical-binding: t -*-
;; Author: Willem Boshoff <boshoffwillem@protonmail.com>
;; URL: https://github.com/boshoffwillem/.emacs.d

;;; Commentary:

;;; Code:
(use-package tabnine
  :hook
  (prog-mode . tabnine-mode)
  (text-mode . tabnine-mode))

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

(use-package apheleia
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
(push '(csharp-ts-mode . csharpier)
      apheleia-mode-alist)
(push '(protobuf-mode . buf)
      apheleia-mode-alist)

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

(use-package sharper
  :bind
  ("C-c n" . sharper-main-transient))

;; .editorconfig files
(use-package editorconfig
  :config
  (editorconfig-mode)
  )

(use-package feature-mode)
(use-package protobuf-mode)

;; (use-package restclient)

(use-package terraform-mode)

;; .xml files
(setq nxml-slash-auto-complete-flag t)
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.nuspec\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xaml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.axaml\\'" . nxml-mode))

;; tree-sitter
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (hcl "https://github.com/mitchellh/tree-sitter-hcl")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (proto "https://github.com/mitchellh/tree-sitter-proto")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-ts-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . ts-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))

(use-package web-mode
  :config
  (setq web-mode-engines-alist
        '(("csharp-ts-mode" . "\\.cshtml\\'")
          ("csharp-ts-mode" . "\\.razor\\."))
        )
  )
(add-to-list 'auto-mode-alist '("\\.cshtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.razor\\'" . web-mode))

;; ;; LSP
;; (use-package lsp-mode
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
;;   (setq lsp-enable-file-watchers nil)
;;   (setq lsp-lens-enable t)
;;   (setq lsp-inlay-hint-enable nil)
;;   (setq lsp-insert-final-newline nil)
;;   (setq lsp-headerline-breadcrumb-enable nil)
;;   (setq lsp-headerline-breadcrumb-enable-symbol-numbers nil)
;;   (setq lsp-modeline-code-actions-enable t)
;;   (setq lsp-modeline-diagnostics-enable t)
;;   (setq lsp-modeline-diagnostics-scope :workspace)
;;   (lsp-eldoc-render-all t)
;;   (lsp-rust-analyzer-cargo-watch-command "clippy")
;;   (lsp-rust-analyzer-server-display-inlay-hints t)
;;   (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
;;   (lsp-rust-analyzer-display-chaining-hints t)
;;   (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
;;   (lsp-rust-analyzer-display-closure-return-type-hints t)
;;   (lsp-rust-analyzer-display-parameter-hints nil)
;;   (lsp-rust-analyzer-display-reborrow-hints nil)
;;   :hook (
;;          (lsp-mode . lsp-enable-which-key-integration)
;;          (bash-ts-mode . lsp-deferred)
;;          (c++-ts-mode . lsp-deferred)
;;          (c-ts-mode . lsp-deferred)
;;          (csharp-ts-mode . lsp-deferred)
;;          (css-ts-mode . lsp-deferred)
;;          (dockerfile-ts-mode . lsp-deferred)
;;          (go-mod-ts-mode . lsp-deferred)
;;          (go-ts-mode . lsp-deferred)
;;          (mhtml-mode . lsp-deferred)
;;          (js-ts-mode . lsp-deferred)
;;          (json-ts-mode . lsp-deferred)
;;          (python-ts-mode . lsp-deferred)
;;          (rust-ts-mode . lsp-deferred)
;;          (terraform-mode . lsp-deferred)
;;          (toml-ts-mode . lsp-deferred)
;;          (tsx-ts-mode . lsp-deferred)
;;          (typescript-ts-mode . lsp-deferred)
;;          (yaml-ts-mode . lsp-deferred)
;;          )
;;   :commands (lsp lsp-deferred))
;;
;; (use-package lsp-treemacs
;;   :config
;;   (lsp-treemacs-sync-mode 1)
;;   :commands lsp-treemacs-errors-list)
;;
;; (use-package consult-lsp
;;   :config
;;   ;; find symbol in project.
;;   ;; (define-key lsp-mode-map (kbd "C-c p t") 'consult-lsp-symbols)
;;   ;; (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
;;   )

(setq image-types '(svg png gif tiff jpeg xpm xbm pbm))
(provide 'ide)

;;; ide.el ends here
