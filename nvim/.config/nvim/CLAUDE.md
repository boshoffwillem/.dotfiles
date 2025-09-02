# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

**IMPORTANT**: When making significant changes to this Neovim configuration (new plugins, architectural changes, new workflows, or major feature additions), update this CLAUDE.md file to keep the documentation current for future Claude Code instances.

## Architecture Overview

This Neovim configuration follows a **modular, domain-specific organization** under the `boshoffwillem` namespace. The architecture prioritizes C#/.NET development while maintaining a clean, extensible structure.

### Core Organization Pattern
- **Entry Point**: `init.lua` sequentially loads all modules in dependency order
- **Plugin Management**: Packer.nvim with auto-bootstrapping and auto-compilation
- **Modular Design**: Each feature area has its own dedicated module in `lua/boshoffwillem/`
- **Home Row Optimization**: Unique `hjkl → jkl;` remapping for movement efficiency

## Key Configuration Modules

### Essential Core Modules
- **`plugins.lua`**: Packer-based plugin ecosystem with extensive C# tooling
- **`lsp.lua`**: Dual LSP setup (OmniSharp + Roslyn) with enhanced semantic tokens and C#-specific code actions
- **`universal-keybinds.lua`**: Universal keybindings that work consistently across all languages
- **`dap.lua`**: Debug configuration with smart .NET executable discovery
- **`csharp.lua`**: Comprehensive C# utilities, keymaps, and IDE-like features
- **`neotest.lua`**: Testing framework with dotnet adapter integration
- **`options.lua`**: Modern Vim defaults with treesitter folding and cross-platform shell support

### Specialized Feature Modules
- **`telescope.lua`**: Project-aware fuzzy finding with predefined work directories (`~/code`, `~/code/work`)
- **`format.lua`**: Conform.nvim primary formatter with language-specific configurations
- **`appearance.lua`**: VSCode theme with transparency and Lualine integration
- **`neo-tree.lua`**: Modern file explorer with git integration, buffer explorer, and project navigation
- **`toggle_term.lua`**: VS Code-style terminal with horizontal bottom layout and multiple keybinding options

## Development Commands

### Plugin Management
```vim
:PackerSync          " Update all plugins
:PackerCompile       " Recompile plugin configuration (auto-triggered on plugins.lua changes)
:Mason               " Manage LSP servers and debugging tools
```

### Universal Keybindings (All Languages)
```vim
" Core Navigation (NEVER override these)
gd                   " Goto definition
gi                   " Goto implementation
gr                   " Find references
gD                   " Goto declaration

" Code Actions & Refactoring (Universal)
<leader>la           " LSP code actions
<leader>lr           " LSP rename
<leader>l=           " LSP format

" Universal Testing
<leader>ctt          " Run current test (intelligent - uses neotest, falls back to language-specific)
<leader>cta          " Run all tests (intelligent - uses neotest, falls back to language-specific)

" Additional LSP
K                    " Hover documentation
<C-k>                " Signature help
<leader>D            " Type definition
```

### C# Development Workflow
```vim
" Testing (language-specific, in addition to universal)
<leader>tn           " Run nearest test (neotest)
<leader>tf           " Run current file tests (neotest)
<leader>tA           " Run all tests (neotest specific)
<leader>dt           " Run current test (dotnet specific)
<leader>dT           " Run all tests (dotnet specific)
<leader>td           " Debug nearest test
<leader>tS           " Toggle test summary window

" Build & Package Management
<leader>db           " Build project/solution
<leader>dc           " Clean project/solution
<leader>dr           " Restore NuGet packages
<leader>da           " Add NuGet package
<leader>dR           " Remove NuGet package

" Code Actions (C#-specific)
<leader>cc           " Apply preferred code action
<leader>cf           " Fix all occurrences
<leader>cg           " Generate code (constructors, properties)
<leader>cr           " Refactor options

" Debugging
F5                   " Start/continue debugging
F10/F11/F12         " Step over/into/out
<leader>lb           " Toggle breakpoint
```

### Project Navigation
```vim
<leader>fp           " Find projects (telescope-project integration)
<leader>ff           " Find files
<leader>fg           " Live grep

" File Explorer (Neo-tree)
<leader>o            " Toggle file explorer sidebar
<leader>O            " Reveal current file in explorer
<leader>ge           " Git status explorer
<leader>be           " Buffer explorer

" Terminal (ToggleTerm)
<C-`>                " Toggle terminal (VS Code style)
<C-\>                " Toggle terminal (alternative)
<Esc>                " Exit terminal mode (when in terminal)
```

## Code Architecture Patterns

### Universal Keybinding System
The configuration uses a universal keybinding system to ensure consistency across all languages:

1. **`universal-keybinds.lua`**: Defines core keybindings that work for ALL languages
2. **LSP Integration**: Universal keybindings are applied via `on_attach` function to every LSP server
3. **Intelligent Test Runner**: Universal test commands (`<leader>ctt`, `<leader>cta`) automatically detect and use the best available test framework

### Module Structure
Most configuration modules follow this pattern:
```lua
local M = {}

function M.setup()
  -- Plugin configuration
end

function M.setup_keymaps()
  -- ONLY language-specific keymaps (universal ones handled by universal-keybinds.lua)
end

return M
```

### LSP Integration
The configuration uses a sophisticated LSP setup:
- **Dual C# LSP support**: Both OmniSharp and Roslyn configured
- **Mason integration**: Automatic LSP server installation and management
- **Enhanced capabilities**: Custom handlers for omnisharp-extended features
- **Semantic token fixes**: Workarounds for OmniSharp semantic highlighting issues

### Plugin Loading Strategy
- **Auto-bootstrapping**: Packer installs itself if missing
- **Lazy loading**: Strategic use of `after`, `requires`, and conditional loading
- **Error handling**: Extensive `pcall()` usage for graceful degradation
- **Auto-compilation**: Plugin configuration recompiles on changes

## Key Dependencies

### Essential External Tools
- **.NET SDK 6.0+**: Required for C# development features
- **Mason-managed tools**: omnisharp, roslyn, netcoredbg (installed via `:Mason`)
- **Git**: Required for Packer plugin management

### Architecture-Specific Notes
- **Cross-platform support**: Windows PowerShell integration in options.lua
- **Project detection**: Smart solution/project file discovery in dotnet-utils.lua
- **Executable discovery**: Automatic .NET build output detection for debugging
- **Test framework support**: xUnit, NUnit, MSTest detection and execution

## Modification Guidelines

### Adding New Language Support
1. Add language-specific LSP server to `lsp.lua` servers table
2. Create dedicated language module if needed (e.g. `lua/boshoffwillem/python.lua`)
3. **NEVER override universal keybindings** - only add language-specific ones
4. Add test runner support to `universal-keybinds.lua` if needed
5. Run `:PackerSync` and `:MasonInstall <lsp-server>`

### Adding New Plugins
1. Add plugin specification to `plugins.lua`
2. Create dedicated configuration module if complex
3. Add module require to `init.lua` in appropriate order
4. Run `:PackerSync` to install

### Extending C# Features
- Modify `csharp.lua` for C#-specific utilities
- Update `lsp.lua` for LSP server changes
- Extend `neotest.lua` for testing modifications
- **NEVER override universal keybindings** - only add C#-specific ones

### LSP Server Management
- Use Mason for server installation: `:MasonInstall <server>`
- Configure servers in the `servers` table in `lsp.lua`
- Add server-specific handlers if needed

## Special Configuration Features

### Unique Customizations
- **Home row movement**: `j/k/l/;` instead of `h/j/k/l` (also applied in Neo-tree file explorer)
- **Universal keybinding system**: Consistent keybindings across all languages
- **Intelligent test runner**: Automatically uses best available test framework
- **Dual LSP approach**: OmniSharp for stability, Roslyn for performance
- **Project-aware debugging**: Automatic .NET executable discovery
- **Comprehensive test integration**: Full neotest workflow with debugging support
- **VS Code-style terminal**: Bottom horizontal terminal with `Ctrl+`` toggle (exactly like VS Code)

### Advanced Integrations
- **Telescope project management**: Custom work directory integration
- **Semantic token handling**: Custom fixes for C# syntax highlighting  
- **Auto-formatting**: Conform.nvim with fallback to LSP formatting
- **Debug adapter intelligence**: Smart configuration selection based on project type