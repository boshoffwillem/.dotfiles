;;; mermaid.el --- mermaid diagrams support  -*- lexical-binding: t; -*-

;; install mermaid cli:
;; npm install -g @mermaid-js/mermaid-cli
(use-package mermaid-mode
  :straight t
  ;; ".mermaid" files: `mermaid-mode' registers this itself via its
  ;; autoloads, but pinning it here makes the mapping explicit and wins
  ;; regardless of load order.
  :mode ("\\.mermaid\\'" . mermaid-mode)
  )

(provide 'mermaid)

;;; mermaid.el ends here
