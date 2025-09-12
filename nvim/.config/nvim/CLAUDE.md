# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

**IMPORTANT**: When making significant changes to this Neovim configuration (new plugins, architectural changes, new workflows, or major feature additions), update this CLAUDE.md file to keep the documentation current for future Claude Code instances.

## Architecture Overview

This Neovim configuration follows a **modular, domain-specific organization** under the `boshoffwillem` namespace. The architecture supports comprehensive multi-language development including C#/.NET, Angular/TypeScript, Vue.js, Go, Rust, Java/Kotlin, and Swift while maintaining a clean, extensible structure.

### Core Organization Pattern
- **Entry Point**: `init.lua` sequentially loads all modules in dependency order
- **Plugin Management**: Packer.nvim with auto-bootstrapping and auto-compilation
- **Modular Design**: Each feature area has its own dedicated module in `lua/boshoffwillem/`
- **Home Row Optimization**: Unique `hjkl → jkl;` remapping for movement efficiency

## Key Configuration Modules

### Essential Core Modules
- **`plugins.lua`**: Packer-based plugin ecosystem with extensive multi-language tooling (C#, Angular, Vue, Go, Rust, Kotlin/Android, Swift/iOS, Flutter)
- **`lsp.lua`**: Multi-language LSP setup (OmniSharp for C#, angularls, vue_ls for Vue, gopls, rust-analyzer, kotlin-language-server, jdtls for Java, sourcekit-lsp for Swift)
- **`universal-keybinds.lua`**: Universal keybindings that work consistently across all languages with intelligent test runner detection
- **`dap.lua`**: Debug configuration with smart executable discovery for .NET, Go (delve), Rust/Swift (codelldb), Node.js, and Android debugging support
- **`csharp.lua`**: Comprehensive C# utilities, keymaps, and IDE-like features
- **`angular.lua`**: Angular CLI integration with TypeScript tooling and project-specific commands
- **`vue.lua`**: Vue.js development with SFC support, Vite/Nuxt integration, and component navigation
- **`go.lua`**: Go development with go.nvim integration, module management, and comprehensive tooling
- **`rust.lua`**: Rust development with rustaceanvim, cargo integration, and crates.nvim support
- **`kotlin.lua`**: Android Kotlin development with Gradle integration, ADB commands, and emulator management
- **`swift.lua`**: iOS Swift development with Xcode integration, simulator management, and Swift Package Manager
- **`neotest.lua`**: Testing framework with adapters for .NET, Jest, Vitest, Go, and Rust
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

### Swift/iOS Development Workflow
```vim
" Xcode Build Commands
<leader>sb           " Build project
<leader>sc           " Clean project  
<leader>sr           " Run app on simulator
<leader>sa           " Archive project

" Testing
<leader>st           " Run all tests
<leader>sT           " Run current test
<leader>ctt          " Universal test runner (works for Swift too)

" iOS Commands
<leader>si           " Install app on simulator/device
<leader>sl           " Show device logs
<leader>sd           " List simulators/devices
<leader>se           " Launch iOS simulator

" Swift Package Manager
<leader>sp           " Package resolve
<leader>su           " Package update
<leader>sP           " Add package dependency
<leader>sv           " Toggle SwiftUI preview (placeholder)
```

### Angular Development Workflow
```vim
" Angular CLI Commands
<leader>ab           " Angular build
<leader>as           " Angular serve
<leader>at           " Angular test
<leader>ae           " Angular e2e tests
<leader>al           " Angular lint
<leader>au           " Angular update

" Code Generation
<leader>agc          " Generate component
<leader>ags          " Generate service
<leader>agd          " Generate directive
<leader>agp          " Generate pipe
<leader>agm          " Generate module

" Package Management
<leader>an           " Install npm package
<leader>aD           " Install dev dependency

" Universal Testing
<leader>ctt          " Universal test runner (works for Angular too)
<leader>cta          " Universal run all tests

" TypeScript Actions (in .ts files)
<leader>to           " Organize imports
<leader>tI           " Add missing imports
<leader>tr           " Remove unused imports
```

### Vue.js Development Workflow
```vim
" Vue/Vite/Nuxt Commands
<leader>vb           " Vue build
<leader>vs           " Vue serve/dev
<leader>vt           " Vue test
<leader>vl           " Vue lint
<leader>vp           " Vue preview

" Nuxt Specific (when detected)
<leader>vn           " Nuxt dev
<leader>vg           " Nuxt generate

" Package Management
<leader>vn           " Install npm package
<leader>vD           " Install dev dependency
<leader>vu           " Update packages

" Vue SFC Navigation (in .vue files)
<leader>vt           " Go to template section
<leader>vs           " Go to script section
<leader>vc           " Go to style section

" Universal Testing
<leader>ctt          " Universal test runner (works for Vue too)
<leader>cta          " Universal run all tests

" Vue Actions
<leader>vo           " Organize imports
```

### Go Development Workflow
```vim
" Go Build and Run
<leader>gb           " Go build
<leader>gr           " Go run (current directory)
<leader>gR           " Go run current file

" Go Testing
<leader>gt           " Go test all packages
<leader>gT           " Go test verbose
<leader>gf           " Go test current package
<leader>ctt          " Universal test runner (works for Go too)

" Go Module Management
<leader>gm           " Go mod tidy
<leader>gd           " Go mod download
<leader>gu           " Go get update

" Go Tools
<leader>gv           " Go vet
<leader>gl           " Go lint (golangci-lint)
<leader>gc           " Go clean cache

" Package Management
<leader>gi           " Go install package
<leader>gg           " Go get package

" Go Actions
<leader>go           " Organize imports
<leader>gI           " Add missing imports

" go.nvim Enhanced Features (when available)
<leader>gta          " Add test for function
<leader>gts          " Add exported test
<leader>gtf          " Test current file
<leader>gtp          " Test current package

" Code Generation
<leader>gsj          " Add json tags
<leader>gsy          " Add yaml tags
<leader>gsr          " Remove tags
<leader>gsf          " Fill struct
<leader>gse          " Fill switch
<leader>gsi          " Add if err

" Coverage
<leader>gca          " Show coverage
<leader>gct          " Toggle coverage
```

### Rust Development Workflow
```vim
" Cargo Commands
<leader>rb           " Cargo build
<leader>rr           " Cargo run
<leader>rR           " Cargo run release
<leader>rt           " Cargo test
<leader>rT           " Cargo test verbose
<leader>ctt          " Universal test runner (works for Rust too)

" Cargo Tools
<leader>rc           " Cargo check
<leader>rl           " Cargo clippy
<leader>rf           " Cargo fmt
<leader>ru           " Cargo update
<leader>rC           " Cargo clean

" Dependency Management
<leader>ra           " Cargo add crate
<leader>rD           " Cargo remove crate

" Workspace
<leader>rw           " Cargo build workspace
<leader>rW           " Cargo test workspace

" Documentation & Benchmarks
<leader>rd           " Cargo doc --open
<leader>rB           " Cargo bench

" rustaceanvim Enhanced Features (when available)
<leader>rh           " Rust hover actions
<leader>re           " Explain error
<leader>rE           " Render diagnostic
<leader>rg           " Crate graph
<leader>rm           " Expand macro
<leader>rp           " Parent module
<leader>rj           " Join lines
<leader>rs           " Structural search replace
<leader>rdb          " Debug
<leader>rdr          " Runnables

" Crates.nvim (in Cargo.toml files)
<leader>ct           " Toggle crates
<leader>cr           " Reload crates
<leader>cv           " Show versions
<leader>cf           " Show features
<leader>cd           " Show dependencies
<leader>cu           " Update crate
<leader>ca           " Update all crates
<leader>cU           " Upgrade crate
<leader>cA           " Upgrade all crates
<leader>cH           " Open homepage
<leader>cR           " Open repository
<leader>cD           " Open documentation
<leader>cC           " Open crates.io
```

### Kotlin/Android Development Workflow
```vim
" Gradle Commands
<leader>kb           " Build project
<leader>kc           " Clean project
<leader>kr           " Run app on device/emulator
<leader>ks           " Sync gradle dependencies

" Testing
<leader>kt           " Run all tests
<leader>kT           " Run current test
<leader>ctt          " Universal test runner (works for Kotlin too)

" Android Commands
<leader>ki           " Install APK on device
<leader>kl           " Show logcat output
<leader>kd           " List connected devices
<leader>ke           " Launch emulator
<leader>ka           " Add dependency to build.gradle

" Java-specific (in .java files)
<leader>jo           " Organize imports
<leader>jv           " Extract variable
<leader>jc           " Extract constant
<leader>jm           " Extract method
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
- **Dual C# LSP support**: OmniSharp configured
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
- **Node.js 16+**: Required for Angular, Vue, and JavaScript/TypeScript development
- **Go 1.19+**: Required for Go development (includes gopls)
- **Rust 1.70+**: Required for Rust development (includes rust-analyzer and cargo)
- **JDK 11 or 17**: Required for Android/Kotlin development
- **Android SDK**: Required for Android development (set `ANDROID_HOME` environment variable)
- **Gradle**: Build tool for Android projects (gradlew preferred)
- **Xcode**: Required for iOS/Swift development (including Command Line Tools)
- **Mason-managed tools**: omnisharp, netcoredbg, angularls, vue_ls, ts_ls, gopls, rust-analyzer, kotlin-language-server, jdtls, ktlint, google-java-format, codelldb, delve, node-debug2-adapter (installed via `:Mason`)
- **Swift tools**: sourcekit-lsp (included with Xcode), swiftformat, swiftlint
- **Angular CLI**: `npm install -g @angular/cli` (for Angular development)
- **Vue CLI**: `npm install -g @vue/cli` (for Vue development, or use Vite)
- **Git**: Required for Packer plugin management

### Architecture-Specific Notes
- **Cross-platform support**: Windows PowerShell integration in options.lua, Android SDK path detection, Node.js path detection
- **Project detection**: Smart project detection for .NET (*.sln, *.csproj), Angular (angular.json), Vue (vite.config.js, vue.config.js), Go (go.mod), Rust (Cargo.toml), Gradle/Maven for Java/Kotlin, Xcode projects for Swift
- **Executable discovery**: Automatic build output detection for .NET, Go, Rust, Android APK installation
- **Test framework support**: xUnit/NUnit/MSTest for C#; Jest/Vitest for Angular/Vue; Go testing for Go; Cargo test for Rust; JUnit/Espresso for Android/Kotlin; XCTest for iOS/Swift
- **Environment integration**: Automatic ANDROID_HOME detection, Go workspace configuration, Rust toolchain detection, Node.js package manager detection
- **Build tool integration**: Automatic detection for gradlew/mvnw (Java/Kotlin), npm/yarn/pnpm (Angular/Vue), cargo (Rust), go commands, xcodebuild and Swift Package Manager (iOS/Swift)

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

### Extending Android/Kotlin Features
- Modify `kotlin.lua` for Kotlin/Android-specific utilities
- Java files are handled via ftplugin/java.lua for enhanced jdtls control
- Update `lsp.lua` for Kotlin/Java LSP server changes
- Android debugging configured in `dap.lua`
- **NEVER override universal keybindings** - only add Kotlin/Java-specific ones

### Extending iOS/Swift Features
- Modify `swift.lua` for Swift/iOS-specific utilities
- Update `lsp.lua` for sourcekit-lsp configuration changes
- iOS debugging configured in `dap.lua` using CodeLLDB
- Xcode project detection and simulator management in `swift.lua`
- **NEVER override universal keybindings** - only add Swift-specific ones

### Extending Angular Features
- Modify `angular.lua` for Angular-specific utilities and CLI integration
- Update `lsp.lua` for angularls configuration changes
- Angular debugging configured in `dap.lua` using Node.js debugger
- Add test runner support to `neotest.lua` (Jest/Vitest adapters already configured)
- **NEVER override universal keybindings** - only add Angular-specific ones

### Extending Vue.js Features
- Modify `vue.lua` for Vue-specific utilities and SFC support
- Update `lsp.lua` for vue_ls configuration changes
- Vue debugging configured in `dap.lua` using Node.js debugger
- Nuxt and Vite project detection and commands in `vue.lua`
- **NEVER override universal keybindings** - only add Vue-specific ones

### Extending Go Features
- Modify `go.lua` for Go-specific utilities and module management
- Update `lsp.lua` for gopls configuration changes
- Go debugging configured in `dap.lua` using delve
- Enhanced features available via go.nvim integration
- **NEVER override universal keybindings** - only add Go-specific ones

### Extending Rust Features
- Modify `rust.lua` for Rust-specific utilities and cargo integration
- Rust-analyzer is handled by rustaceanvim plugin
- Rust debugging configured in `dap.lua` using CodeLLDB
- Crates.nvim provides advanced dependency management for Cargo.toml
- **NEVER override universal keybindings** - only add Rust-specific ones

### LSP Server Management
- Use Mason for server installation: `:MasonInstall <server>`
- Configure servers in the `servers` table in `lsp.lua`
- Add server-specific handlers if needed

## Special Configuration Features

### Unique Customizations
- **Home row movement**: `j/k/l/;` instead of `h/j/k/l` (also applied in Neo-tree file explorer)
- **Universal keybinding system**: Consistent keybindings across all languages (C#, Angular, Vue, Go, Rust, Java/Kotlin, Swift)
- **Intelligent test runner**: Automatically detects and uses best available test framework (Jest/Vitest for Angular/Vue, Go test, Cargo test, Gradle/Maven for Java/Kotlin)
- **Multi-language LSP**: OmniSharp for C#, angularls for Angular, vue_ls for Vue, gopls for Go, rust-analyzer for Rust, kotlin-language-server for Kotlin, jdtls for Java, sourcekit-lsp for Swift
- **Project-aware debugging**: Automatic executable discovery for .NET, Go (delve), Rust (codelldb), Node.js applications, Android app attachment, and iOS simulator debugging
- **Comprehensive test integration**: Full neotest workflow with adapters for .NET, Jest, Vitest, Go, Rust, and debugging support for all languages
- **VS Code-style terminal**: Bottom horizontal terminal with `Ctrl+`` toggle (exactly like VS Code)
- **Modern web development**: Full Angular CLI integration, Vue.js SFC support, TypeScript tooling
- **Systems programming**: Advanced Go tooling with go.nvim, comprehensive Rust support with rustaceanvim and crates.nvim
- **Android development**: Integrated ADB commands, emulator management, and Gradle integration
- **iOS development**: Integrated Xcode commands, simulator management, and Swift Package Manager
- **Flutter support**: Full Flutter development with hot reload and widget inspection for both Android and iOS

### Advanced Integrations
- **Telescope project management**: Custom work directory integration
- **Semantic token handling**: Custom fixes for C# syntax highlighting  
- **Auto-formatting**: Conform.nvim with fallback to LSP formatting
- **Debug adapter intelligence**: Smart configuration selection based on project type
