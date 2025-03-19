;;; Commentary:

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
;; (with-eval-after-load 'apheleia
;;   (add-to-list 'apheleia-formatters
;;                '(terraform-mode . "terraform fmt -")))
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

;; (use-package dap-mode
;;   :commands (dap-debug dap-breakpoints-add)
;;   :init
;;   (dap-mode 1)
;;   (dap-ui-mode 1)
;;   (dap-auto-configure-mode)
;;   (require 'dap-netcore)
;;   :custom
;;   (dap-netcore-install-dir "~/.config/emacs/.cache/"))

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

;; .editorconfig files
;; (use-package editorconfig
;;   :config
;;   (editorconfig-mode)
;;   )

(use-package feature-mode)
(use-package protobuf-mode)
(use-package powershell)
(use-package terraform-mode)
(add-to-list 'auto-mode-alist '("\\.tf\\'" . terraform-mode))
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))

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
             (elisp "https://github.com/Wilfred/tree-sitter-elisp")
             (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
             (hcl "https://github.com/mitchellh/tree-sitter-hcl")
             (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "src")
             (json "https://github.com/tree-sitter/tree-sitter-json")
             (make "https://github.com/alemuller/tree-sitter-make")
             (markdown "https://github.com/ikatyang/tree-sitter-markdown")
             (proto "https://github.com/mitchellh/tree-sitter-proto")
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

;; (dolist (mapping
;;          '((python-mode . python-ts-mode)
;;            (css-mode . css-ts-mode)
;;            (typescript-mode . typescript-ts-mode)
;;            (js-mode . typescript-ts-mode)
;;            (js2-mode . typescript-ts-mode)
;;            (c-mode . c-ts-mode)
;;            (c++-mode . c++-ts-mode)
;;            (c-or-c++-mode . c-or-c++-ts-mode)
;;            (bash-mode . bash-ts-mode)
;;            (css-mode . css-ts-mode)
;;            (json-mode . json-ts-mode)
;;            (js-json-mode . json-ts-mode)
;;            (sh-mode . bash-ts-mode)
;;            (sh-base-mode . bash-ts-mode)
;;            (yaml-mode . yaml-ts-mode)
;;            ))
;;   (add-to-list 'major-mode-remap-alist mapping))

;; (os/setup-install-grammars)
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))

(use-package lsp-mode
  :diminish "LSP"
  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((
           csharp-mode
           tsx-ts-mode
           typescript-ts-mode
           js-ts-mode
           web-mode
           yaml-ts-mode
           ) . lsp-deferred))
  :custom
  (lsp-keymap-prefix "C-c l")           ; Prefix for LSP actions
  (lsp-completion-provider :none)       ; Using Corfu as the provider
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil)                      ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil)        ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)                  ; Debounce timer for `after-change-function'
  ;; core
  (lsp-enable-xref t)                   ; Use xref to find references
  (lsp-auto-configure t)                ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t)            ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)              ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-enable-indentation nil)          ; I use prettier
  (lsp-enable-links nil)                ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t)     ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)   ; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil)      ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)                         ; Important to provide full JSX completion
  (lsp-completion-show-kind t)                   ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t)  ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil)  ; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1)                ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)              ; Show docs for symbol at point
  (lsp-eldoc-render-all nil)            ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil)      ; Related to highlighting, and we defer to treesitter

  ;; :init
  ;; (setq lsp-use-plists t)
  )

(use-package lsp-ui
  :ensure t
  :commands
  (lsp-ui-doc-show
   lsp-ui-doc-glance)
  :bind (:map lsp-mode-map
              ("C-c C-d" . 'lsp-ui-doc-glance))
  :after (lsp-mode evil)
  :config (setq lsp-ui-doc-enable t
                evil-lookup-func #'lsp-ui-doc-glance ; Makes K in evil-mode toggle the doc for symbol at point
                lsp-ui-doc-show-with-cursor nil      ; Don't show doc when cursor is over symbol - too distracting
                lsp-ui-doc-include-signature t       ; Show signature
                lsp-ui-doc-position 'at-point))

;; (use-package lsp-treemacs
;;   :config
;;   (lsp-treemacs-sync-mode 1)
;;   :commands lsp-treemacs-errors-list)

;; (use-package consult-lsp
;;   :config
;;   ;; find symbol in project.
;;   ;; (define-key lsp-mode-map (kbd "C-c p t") 'consult-lsp-symbols)
;;   ;; (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
;;   )

(setq image-types '(svg png gif tiff jpeg xpm xbm pbm))
(provide 'ide)

;;; ide.el ends here
