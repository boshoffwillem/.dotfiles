;;; zig-treesit.el --- shared tree-sitter grammar build setup  -*- lexical-binding: t; -*-

;; Centralizes how every tree-sitter grammar in this config gets built: on
;; Windows, where a real C/C++ toolchain (MSVC/mingw) isn't guaranteed to be
;; on PATH, route through zig's bundled clang via the zig-cc.cmd/zig-c++.cmd
;; wrapper scripts next to this file (see those for why a wrapper script is
;; needed instead of a bare "zig cc" string -- `call-process' never
;; shell-splits its PROGRAM argument, and zig always emits .obj instead of
;; .o on Windows, which treesit.el's link step doesn't glob for). On other
;; platforms CC/C++ are left nil so treesit's own auto-detection
;; (cc/gcc/c99) is used, since this same config also runs on Linux via
;; install_debian.sh et al., where a real compiler is already expected to be
;; present.
;;
;; Language/framework files call `my/treesit-register-grammar' instead of
;; touching `treesit-language-source-alist' directly, so this policy (and
;; the fact that `treesit-language-source-alist' isn't defined until
;; treesit.el is actually loaded -- `add-to-list' on it too early signals
;; void-variable, which aborts the rest of init.el's loading too) only has
;; to be handled once.

(require 'treesit)

(defun my/treesit-register-grammar (lang url &optional revision source-dir)
  "Register a tree-sitter grammar recipe for LANG, built via zig on Windows."
  (let ((cc (and (eq system-type 'windows-nt) (locate-user-emacs-file "zig-cc.cmd")))
        (c++ (and (eq system-type 'windows-nt) (locate-user-emacs-file "zig-c++.cmd"))))
    (add-to-list 'treesit-language-source-alist
                 (list lang url revision source-dir cc c++))))

(provide 'zig-treesit)
;;; zig-treesit.el ends here
