;;; csharp.el --- C# tree-sitter + LSP (Roslyn) support  -*- lexical-binding: t; -*-

;; Mirrors the Neovim setup (lua/plugins/lsp.lua): the actual Microsoft
;; Roslyn language server (Microsoft.CodeAnalysis.LanguageServer), not
;; OmniSharp. lsp-mode ships this as its own built-in client
;; (clients/lsp-roslyn.el, server-id `csharp-roslyn'), separate from its
;; older OmniSharp-based lsp-csharp.el client. It activates on the "csharp"
;; LSP language-id, which core Emacs already maps both `csharp-mode' and
;; `csharp-ts-mode' to, so no custom client registration is needed here --
;; just `dotnet' on PATH (lsp-roslyn downloads/manages the actual server
;; package itself via NuGet, unlike Neovim's `dotnet tool install -g
;; roslyn-language-server').
;;
;; Core Emacs ships `csharp-ts-mode' (tree-sitter), but unlike
;; typescript-ts-mode it doesn't ship a graceful "-maybe" fallback wrapper
;; or an autoload-time auto-mode-alist entry: invoking it without the
;; grammar installed signals an error outright, and .cs only maps to the
;; legacy `csharp-mode' until you've switched into `csharp-ts-mode' at least
;; once yourself. `csharp-mode' is a mature, fully-featured major mode in
;; its own right (not a bare fallback like fundamental-mode), so mirror the
;; "-maybe" pattern here rather than relying on core's incomplete one.
;;
;; One-time setup per machine: M-x treesit-install-language-grammar RET
;; c-sharp (uses the recipe below; see zig-treesit.el for why CC/C++ point
;; at zig on Windows).

(require 'zig-treesit (locate-user-emacs-file "zig-treesit"))
(require 'lsp-setup (locate-user-emacs-file "lsp-setup"))

(my/treesit-register-grammar
 'c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp" "master")

(defun my/csharp-mode-maybe ()
  "Use tree-sitter `csharp-ts-mode' if its grammar is installed, else `csharp-mode'."
  (if (treesit-language-available-p 'c-sharp)
      (csharp-ts-mode)
    (csharp-mode)))

(add-to-list 'auto-mode-alist '("\\.cs\\'" . my/csharp-mode-maybe))
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.fsproj\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.slnx\\'" . nxml-mode))

;; `csharp-ts-mode-indent-offset'/`c-basic-offset' default to 4 already,
;; matching .NET convention -- unlike Vue/React, there's no shared house
;; style to apply here, so indentation is left untouched.

;; (add-hook 'csharp-mode-hook #'lsp-deferred)
;; (add-hook 'csharp-ts-mode-hook #'lsp-deferred)

;;; csharp.el ends here
