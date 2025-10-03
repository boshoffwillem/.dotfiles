# C# and .NET Development Setup for Neovim

This document outlines the enhanced C# and .NET development setup for your Neovim configuration.

## New Plugins Added

### Core C# Support
- **omnisharp-extended-lsp.nvim** - Enhanced OmniSharp features including better go-to-definition
- **roslyn.nvim** - Microsoft's new Roslyn LSP server (alternative to OmniSharp)
- **lsp-overloads.nvim** - Shows method overloads in completion and signature help
- **csharp.nvim** - Additional C# utilities and configurations

### Testing & Debugging
- **neotest-dotnet** - Already configured, now with enhanced settings for better test running
- Enhanced DAP configuration for .NET debugging with automatic executable discovery

## Key Features Added

### 1. Enhanced LSP Configuration
- **OmniSharp improvements:**
  - Better inlay hints for parameters and types
  - Roslyn analyzer support
  - Import completion
  - Organize imports on format
  - EditorConfig support

- **Roslyn LSP support:**
  - Alternative to OmniSharp with better performance
  - Advanced semantic highlighting
  - Improved code analysis

### 2. C# Specific Keymaps
**Code Actions:**
- `<leader>cc` - Apply preferred code action
- `<leader>cf` - Fix all occurrences
- `<leader>cg` - Generate code (constructors, properties, etc.)
- `<leader>cr` - Refactor options

**Testing:**
- `<leader>tn` - Run nearest test
- `<leader>tf` - Run tests in current file
- `<leader>ta` - Run all tests
- `<leader>td` - Debug nearest test
- `<leader>ts` - Stop running tests
- `<leader>to` - Show test output
- `<leader>tO` - Toggle output panel
- `<leader>tS` - Toggle test summary
- `<leader>tw` - Toggle watch mode
- `]t` / `[t` - Jump to next/previous failed test

**Build & Package Management:**
- `<leader>db` - Build project/solution
- `<leader>dc` - Clean project/solution
- `<leader>dr` - Restore packages
- `<leader>dt` - Run tests
- `<leader>dT` - Run current test method
- `<leader>dw` - Watch tests
- `<leader>da` - Add package
- `<leader>dR` - Remove package
- `<leader>dn` - Create new project

**File Creation:**
- `<leader>nc` - Create new class
- `<leader>ni` - Create new interface

### 3. Intelligent Project Detection
- Automatically finds solution (.sln) files
- Falls back to project (.csproj/.fsproj) files
- Smart executable detection for debugging
- Project-aware test running

### 4. Enhanced Test Runner
- **neotest-dotnet** configured with:
  - Solution-based discovery
  - Support for xUnit, NUnit, MSTest
  - Debugging integration
  - Watch mode
  - Rich test output
  - Test summary window

### 5. Improved Debugging
- **Automatic executable discovery** for .NET apps
- Support for .NET 6, 7, and 8
- Launch configurations for different scenarios
- Process attachment support
- Better DAP configuration

### 6. C# File Settings
- Auto-formatting on save
- Proper indentation (4 spaces)
- Line length at 120 characters
- Inlay hints enabled
- C#-specific settings applied automatically

## Setup Instructions

### 1. Install Dependencies
Make sure you have the following installed:

```bash
# .NET SDK (version 6.0 or higher recommended)
dotnet --version

# OmniSharp (via Mason)
:MasonInstall omnisharp

# Roslyn LSP (alternative, install if you prefer it over OmniSharp)
:MasonInstall roslyn

# NetCoreDbg for debugging
:MasonInstall netcoredbg
```

### 2. Plugin Installation
Run the following in Neovim:
```vim
:PackerSync
```

### 3. Choose Your LSP
You can use either OmniSharp or Roslyn LSP:

- **OmniSharp**: More mature, widely used
- **Roslyn**: Microsoft's new official LSP, better performance

Both are configured. To disable one, edit `lua/boshoffwillem/lsp.lua`.

## Usage Tips

### Testing
1. Open a test file
2. Use `<leader>tS` to open the test summary
3. Use `<leader>tn` to run the test under cursor
4. Use `<leader>td` to debug a test

### Debugging
1. Set breakpoints with `<leader>lb`
2. Start debugging with `F5`
3. The debugger will automatically find your executable
4. Use `F10` (step over), `F11` (step into), `F12` (step out)

### Code Actions
1. Place cursor on code that has suggestions
2. Use `<leader>cc` for quick fixes
3. Use `<leader>cg` for code generation (constructors, properties)
4. Use `<leader>cr` for refactoring options

### Project Management
1. Use `<leader>db` to build your project
2. Use `<leader>da` to add NuGet packages
3. Use `<leader>dn` to create new projects

## Configuration Files

- `lua/boshoffwillem/csharp.lua` - Main C# configuration and utilities
- `lua/boshoffwillem/neotest.lua` - Test runner configuration
- `lua/boshoffwillem/dotnet-utils.lua` - .NET project utilities
- `lua/boshoffwillem/lsp.lua` - Enhanced LSP settings for C#
- `lua/boshoffwillem/dap.lua` - Enhanced debugging configuration

## Troubleshooting

### LSP Issues
- Check `:LspInfo` to see if OmniSharp/Roslyn is running
- Restart LSP with `:LspRestart`
- Check Mason installations with `:Mason`

### Test Issues
- Ensure you're in a project with test files
- Check neotest summary with `<leader>tS`
- Verify test framework attributes are recognized

### Debug Issues
- Ensure netcoredbg is installed via Mason
- Build your project before debugging
- Check that the executable path is correct

This setup transforms your Neovim into a powerful C# IDE with intellisense, testing, debugging, and project management capabilities.
