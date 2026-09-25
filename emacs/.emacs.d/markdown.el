;;; markdown.el --- Markdown editing + preview support  -*- lexical-binding: t; -*-

;; `markdown-mode' has no renderer of its own -- previews shell out to
;; `markdown-command' and show the resulting HTML. pandoc (`brew install
;; pandoc') is used here, reading GitHub-flavored Markdown so tables, task
;; lists and fenced code blocks render the same way they do on GitHub.
;; No `--standalone': markdown-mode wraps the output in its own <html>/<head>
;; itself, and pandoc's standalone mode warns on stderr about a missing
;; <title> for most READMEs.
;;
;; Preview commands:
;;   C-c C-c l      toggle `markdown-live-preview-mode' -- rendered in an eww
;;                  window beside the source, re-rendered on every save
;;   C-c C-c p      render and open in the system browser
;;   C-c C-x C-m    toggle hiding markup (**, [](), ...) in the source buffer
;;
;; README.md files open in `gfm-mode' (GitHub-flavored variant of
;; `markdown-mode'), since that's what they're rendered as on GitHub anyway.

(use-package markdown-mode
  :straight t
  ;; Each entry is pushed onto the front of `auto-mode-alist' in order, so
  ;; the more specific README pattern has to come last to win over "\\.md".
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :custom
  (markdown-command '("pandoc" "--from=gfm" "--to=html5"))
  (markdown-split-window-direction 'right)
  (markdown-fontify-code-blocks-natively t))

;;; markdown.el ends here
