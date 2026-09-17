;;; vue.el --- Vue.js single-file component support  -*- lexical-binding: t; -*-

;; Vue SFCs mix template/script/style markup in one file. web-mode parses
;; each region with its own sub-mode (html/js/css) while staying in a single
;; major mode -- which is what vue-language-server (wired up in
;; lsp-setup.el) expects: LSP keys a buffer's server off its one
;; major-mode/languageId, and Volar/vue-language-server does the
;; template/script/style splitting itself, server-side. The `vue-mode'
;; package's mmm-mode approach instead fakes multiple *real* major modes in
;; one buffer via overlays, which conflicts with that model (and is less
;; actively maintained than web-mode).
;;
;; web-mode itself is installed/configured generically in frontend.el.
;;
;; lsp-mode's Volar client (`vue-semantic-server') activates purely by
;; filename suffix (.vue), same as its ts-ls client -- neither cares that
;; the buffer's major-mode is web-mode rather than some Vue-specific mode,
;; so plain `lsp-deferred' is enough; no custom client registration needed.
;; Also make sure `npm install -g @vue/language-server` is run for LSP
;; support (Volar wraps/depends on it).
;;
;; @vue/language-server declares its `typescript' dependency as "latest",
;; and TypeScript 7 dropped the classic `ts.server' API entirely (part of
;; its native/Corsa rewrite) -- an unpinned install silently pulls TS 7 into
;; its own nested node_modules/typescript, and the server crashes on the
;; first file open with "Cannot read properties of undefined (reading
;; 'protocol')" (see *vue-semantic-server::stderr*). If a reinstall/upgrade
;; of @vue/language-server ever reintroduces this, fix it with:
;;   cd <npm root>/node_modules/@vue/language-server && npm install typescript@5.7.3 --no-save
;; (same underlying issue, and same version pin, as ts-ls's `typescript'
;; dependency in react.el.)

(require 'frontend (locate-user-emacs-file "frontend"))
(require 'lsp-setup (locate-user-emacs-file "lsp-setup"))

(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

(defun my/vue-web-mode-setup ()
  "Apply Vue-specific web-mode settings, scoped to .vue buffers only.
web-mode's indent/behavior variables are global defcustoms shared with
unrelated file types (.php, .html, .erb, ...) configured in frontend.el, so
these are set buffer-locally here rather than via a blanket `setq'. LSP is
started the same way, scoped to .vue, so unrelated web-mode buffers (.php,
.html, .erb, ...) don't get an LSP client attached."
  (when (and buffer-file-name (string-match-p "\\.vue\\'" buffer-file-name))
    (setq-local web-mode-markup-indent-offset my/frontend-indent-width
                web-mode-css-indent-offset my/frontend-indent-width
                web-mode-code-indent-offset my/frontend-indent-width
                web-mode-script-padding 0
                web-mode-style-padding 0
                web-mode-enable-auto-quoting nil
                web-mode-enable-current-element-highlight t)
    (lsp-deferred)))

(add-hook 'web-mode-hook #'my/vue-web-mode-setup)

;;; vue.el ends here
