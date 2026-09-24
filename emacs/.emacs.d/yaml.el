;;; yaml.el --- YAML tree-sitter + LSP (yaml-language-server) support  -*- lexical-binding: t; -*-

;; Same "-maybe" pattern as csharp.el: core Emacs ships `yaml-ts-mode', but
;; invoking it without the grammar installed signals an error, so fall back
;; to the `yaml-mode' package when the grammar isn't there yet.
;;
;; lsp-mode's built-in yamlls client (clients/lsp-yaml.el) activates on
;; both modes. It needs `yaml-language-server' on PATH (`npm install -g
;; yaml-language-server'); if missing, lsp-mode offers to npm-install it.
;;
;; Formatting goes through apheleia (prettier), set up in init.el.
;;
;; One-time setup per machine: M-x treesit-install-language-grammar RET yaml

(require 'zig-treesit (locate-user-emacs-file "zig-treesit"))
(require 'lsp-setup (locate-user-emacs-file "lsp-setup"))

(my/treesit-register-grammar
 'yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml" "master")

(use-package yaml-mode
  :straight t)

(defun my/yaml-mode-maybe ()
  "Use tree-sitter `yaml-ts-mode' if its grammar is installed, else `yaml-mode'."
  (if (treesit-language-available-p 'yaml)
      (yaml-ts-mode)
    (yaml-mode)))

;; After the `yaml-mode' use-package above, so this entry lands in front of
;; the one yaml-mode's autoloads add.
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . my/yaml-mode-maybe))

;; (add-hook 'yaml-mode-hook #'lsp-deferred)
;; (add-hook 'yaml-ts-mode-hook #'lsp-deferred)

;;; yaml.el ends here
