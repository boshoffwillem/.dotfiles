;;; dart.el --- Dart/Flutter major mode + LSP support  -*- lexical-binding: t; -*-

;; The Dart analysis server ships inside the Dart SDK (`dart
;; language-server'), which itself ships inside the Flutter SDK -- so a
;; Flutter install is all that's needed. `lsp-dart' registers the client and
;; finds the SDK via the `flutter'/`dart' executables on PATH; it has to be
;; loaded (not just installed) for lsp-mode to know about it.
;;
;; `flutter' adds `flutter-run-or-hot-reload' (C-M-x) and
;; `flutter-test-mode' for running tests from test files.
;;
;; Formatting goes through apheleia (`dart format'), set up in init.el.

(require 'lsp-setup (locate-user-emacs-file "lsp-setup"))

(use-package dart-mode
  :straight t
  ;; :hook (dart-mode . lsp-deferred)
  )

;; (use-package lsp-dart
;;   :straight t
;;   :after lsp-mode)

(use-package flutter
  :straight t
  :after dart-mode
  :hook (dart-mode . flutter-test-mode)
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "~/development/flutter/"))

;;; dart.el ends here
