local M = {}

function M.setup()
  -- Rust-specific configuration
  
  -- Set up Rust file settings
  vim.api.nvim_create_autocmd("FileType", {
    pattern = "rust",
    callback = function()
      -- Rust prefers 4 spaces
      vim.opt_local.expandtab = true
      vim.opt_local.tabstop = 4
      vim.opt_local.shiftwidth = 4
      vim.opt_local.softtabstop = 4
      
      -- Auto-format on save (handled by rustaceanvim if available)
      if not pcall(require, "rustaceanvim") then
        local group = vim.api.nvim_create_augroup("RustFormat", { clear = false })
        vim.api.nvim_create_autocmd("BufWritePre", {
          pattern = "*.rs",
          group = group,
          callback = function()
            vim.lsp.buf.format({ async = false })
          end,
        })
      end
    end
  })
  
  -- Set up universal keybindings when rust_analyzer attaches
  -- This is needed because rustaceanvim handles rust-analyzer setup independently
  vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(args)
      local client = vim.lsp.get_client_by_id(args.data.client_id)
      if client and client.name == "rust_analyzer" then
        require("boshoffwillem.universal-keybinds").setup_lsp_keybinds(client, args.buf)
      end
    end,
  })
  
  -- Rust project detection
  local function is_rust_project()
    return vim.fn.filereadable("Cargo.toml") == 1
  end
  
  -- Set up crates.nvim if available
  local has_crates, crates = pcall(require, "crates")
  if has_crates then
    crates.setup({
      null_ls = {
        enabled = false,
      },
      popup = {
        autofocus = true,
        hide_on_select = true,
        copy_register = '"',
        style = "minimal",
        border = "none",
        show_version_date = false,
        show_dependency_version = true,
        max_height = 30,
        min_width = 20,
        padding = 1,
      },
      completion = {
        cmp = {
          enabled = true,
        },
      },
    })
    
    -- Set up crates.nvim autocommands
    vim.api.nvim_create_autocmd("FileType", {
      pattern = "toml",
      callback = function()
        if vim.fn.expand("%:t") == "Cargo.toml" then
          crates.show()
        end
      end,
    })
  end
  
  -- Configure rustaceanvim if available (it auto-configures rust-analyzer)
  -- Note: rustaceanvim v4+ uses vim.g.rustaceanvim for configuration
  vim.g.rustaceanvim = {
    -- Plugin configuration
    tools = {
      -- rust-tools options
      
      -- how to execute terminal commands
      -- options right now: termopen / quickfix / toggleterm / vimux
      executor = "toggleterm",
      
      -- callback to execute once rust-analyzer is done initializing the workspace
      -- The callback receives one parameter indicating the `health` of the server: "ok" | "warning" | "error"
      on_initialized = nil,
      
      -- automatically call RustReloadWorkspace when writing to a Cargo.toml file.
      reload_workspace_from_cargo_toml = true,
      
      -- These apply to the default RustSetInlayHints command
      inlay_hints = {
        -- automatically set inlay hints (type hints)
        -- default: true
        auto = true,
        -- Only show inlay hints for the current line
        only_current_line = false,
        -- whether to show parameter hints with the inlay hints or not
        show_parameter_hints = true,
        -- prefix for parameter hints
        parameter_hints_prefix = "<- ",
        -- prefix for all the other hints (type, chaining)
        other_hints_prefix = "=> ",
        -- whether to align to the length of the longest line in the file
        max_len_align = false,
        -- padding from the left if max_len_align is true
        max_len_align_padding = 1,
        -- whether to align to the extreme right or not
        right_align = false,
        -- padding from the right if right_align is true
        right_align_padding = 7,
        -- The color of the hints
        highlight = "Comment",
      },
      
      -- options same as lsp hover / vim.lsp.util.open_floating_preview()
      hover_actions = {
        -- the border that is used for the hover window
        -- see vim.api.nvim_open_win()
        border = {
          { "╭", "FloatBorder" },
          { "─", "FloatBorder" },
          { "╮", "FloatBorder" },
          { "│", "FloatBorder" },
          { "╯", "FloatBorder" },
          { "─", "FloatBorder" },
          { "╰", "FloatBorder" },
          { "│", "FloatBorder" },
        },
        
        -- Maximal width of the hover window. Nil means no max.
        max_width = nil,
        
        -- Maximal height of the hover window. Nil means no max.
        max_height = nil,
        
        -- whether the hover action window gets automatically focused
        -- default: false
        auto_focus = false,
      },
      
      -- settings for showing the crate graph based on graphviz and the dot
      -- command
      crate_graph = {
        -- Backend used for displaying the graph
        -- see: https://graphviz.org/docs/outputs/
        -- default: x11
        backend = "x11",
        -- where to store the output, nil for no output stored (relative
        -- path from pwd)
        -- default: nil
        output = nil,
        -- true for all crates.io and external crates, false only the local
        -- crates
        -- default: true
        full = true,
        
        -- List of backends found on: https://graphviz.org/docs/outputs/
        -- Is used for input validation and autocompletion
        -- Last updated: 2021-08-26
        enabled_graphviz_backends = {
          "bmp",
          "cgimage",
          "canon",
          "dot",
          "gv",
          "xdot",
          "xdot1.2",
          "xdot1.4",
          "eps",
          "exr",
          "fig",
          "gd",
          "gd2",
          "gif",
          "gtk",
          "ico",
          "imap",
          "cmapx",
          "imap_np",
          "cmapx_np",
          "ismap",
          "jp2",
          "jpg",
          "jpeg",
          "jpe",
          "json",
          "json0",
          "dot_json",
          "xdot_json",
          "pdf",
          "pic",
          "pct",
          "pict",
          "plain",
          "plain-ext",
          "png",
          "pov",
          "ps",
          "ps2",
          "psd",
          "sgi",
          "svg",
          "svgz",
          "tga",
          "tiff",
          "tif",
          "tk",
          "vml",
          "vmlz",
          "wbmp",
          "webp",
          "xlib",
          "x11",
        },
      },
    },
    
    -- LSP configuration
    server = {
      -- use universal on_attach from lsp.lua
      on_attach = function(client, bufnr)
        -- Universal keybindings are handled by LspAttach autocmd above
        -- This function may not be called by rustaceanvim v4+
      end,
      default_settings = {
        -- rust-analyzer language server configuration
        ["rust-analyzer"] = {
          -- enable clippy on save
          checkOnSave = {
            command = "clippy",
          },
          cargo = {
            allFeatures = true,
            loadOutDirsFromCheck = true,
            runBuildScripts = true,
          },
          -- Add clippy lints for Rust
          procMacro = {
            enable = true,
            ignored = {
              ["async-trait"] = { "async_trait" },
              ["napi-derive"] = { "napi" },
              ["async-recursion"] = { "async_recursion" },
            },
          },
        },
      },
    },
    
    -- DAP configuration
    dap = {
      adapter = {
        type = "executable",
        command = "lldb-vscode",
        name = "rt_lldb",
      },
    },
  }
end

function M.setup_keymaps()
  local opts = { noremap = true, silent = true }
  
  -- Rust-specific keymaps (available in all Rust files)
  vim.api.nvim_create_autocmd("FileType", {
    pattern = "rust",
    callback = function()
      -- Cargo commands
      vim.keymap.set("n", "<leader>rb", ":ToggleTerm cmd='cargo build'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Cargo build", buffer = true }))
      vim.keymap.set("n", "<leader>rr", ":ToggleTerm cmd='cargo run'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Cargo run", buffer = true }))
      vim.keymap.set("n", "<leader>rR", ":ToggleTerm cmd='cargo run --release'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Cargo run release", buffer = true }))
      vim.keymap.set("n", "<leader>rt", ":ToggleTerm cmd='cargo test'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Cargo test", buffer = true }))
      vim.keymap.set("n", "<leader>rT", ":ToggleTerm cmd='cargo test -- --nocapture'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Cargo test verbose", buffer = true }))
      
      -- Cargo check and clippy
      vim.keymap.set("n", "<leader>rc", ":ToggleTerm cmd='cargo check'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Cargo check", buffer = true }))
      vim.keymap.set("n", "<leader>rl", ":ToggleTerm cmd='cargo clippy'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Cargo clippy", buffer = true }))
      vim.keymap.set("n", "<leader>rf", ":ToggleTerm cmd='cargo fmt'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Cargo fmt", buffer = true }))
      
      -- Cargo management
      vim.keymap.set("n", "<leader>ru", ":ToggleTerm cmd='cargo update'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Cargo update", buffer = true }))
      vim.keymap.set("n", "<leader>rC", ":ToggleTerm cmd='cargo clean'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Cargo clean", buffer = true }))
      
      -- Cargo add/remove dependencies
      vim.keymap.set("n", "<leader>ra", function()
        local crate = vim.fn.input("Crate to add: ")
        if crate ~= "" then
          vim.cmd("ToggleTerm cmd='cargo add " .. crate .. "'")
        end
      end, vim.tbl_extend("force", opts, { desc = "Cargo add crate", buffer = true }))
      
      vim.keymap.set("n", "<leader>rD", function()
        local crate = vim.fn.input("Crate to remove: ")
        if crate ~= "" then
          vim.cmd("ToggleTerm cmd='cargo remove " .. crate .. "'")
        end
      end, vim.tbl_extend("force", opts, { desc = "Cargo remove crate", buffer = true }))
      
      -- Cargo workspace
      vim.keymap.set("n", "<leader>rw", ":ToggleTerm cmd='cargo build --workspace'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Cargo build workspace", buffer = true }))
      vim.keymap.set("n", "<leader>rW", ":ToggleTerm cmd='cargo test --workspace'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Cargo test workspace", buffer = true }))
      
      -- Documentation
      vim.keymap.set("n", "<leader>rd", ":ToggleTerm cmd='cargo doc --open'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Cargo doc", buffer = true }))
      
      -- Benchmarks
      vim.keymap.set("n", "<leader>rB", ":ToggleTerm cmd='cargo bench'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Cargo bench", buffer = true }))
      
      -- Check if rustaceanvim is available for enhanced features
      local has_rustaceanvim = pcall(require, "rustaceanvim")
      if has_rustaceanvim then
        -- rustaceanvim specific keymaps
        vim.keymap.set("n", "<leader>rh", function()
          vim.cmd.RustLsp("hover", "actions")
        end, vim.tbl_extend("force", opts, { desc = "Rust hover actions", buffer = true }))
        
        vim.keymap.set("n", "<leader>re", function()
          vim.cmd.RustLsp("explainError")
        end, vim.tbl_extend("force", opts, { desc = "Explain error", buffer = true }))
        
        vim.keymap.set("n", "<leader>rE", function()
          vim.cmd.RustLsp("renderDiagnostic")
        end, vim.tbl_extend("force", opts, { desc = "Render diagnostic", buffer = true }))
        
        vim.keymap.set("n", "<leader>rg", function()
          vim.cmd.RustLsp("crateGraph")
        end, vim.tbl_extend("force", opts, { desc = "Crate graph", buffer = true }))
        
        vim.keymap.set("n", "<leader>rm", function()
          vim.cmd.RustLsp("expandMacro")
        end, vim.tbl_extend("force", opts, { desc = "Expand macro", buffer = true }))
        
        vim.keymap.set("n", "<leader>rp", function()
          vim.cmd.RustLsp("parentModule")
        end, vim.tbl_extend("force", opts, { desc = "Parent module", buffer = true }))
        
        vim.keymap.set("n", "<leader>rj", function()
          vim.cmd.RustLsp("joinLines")
        end, vim.tbl_extend("force", opts, { desc = "Join lines", buffer = true }))
        
        vim.keymap.set("n", "<leader>rs", function()
          vim.cmd.RustLsp("ssr")
        end, vim.tbl_extend("force", opts, { desc = "Structural search replace", buffer = true }))
        
        -- Debug
        vim.keymap.set("n", "<leader>rdb", function()
          vim.cmd.RustLsp("debuggables")
        end, vim.tbl_extend("force", opts, { desc = "Debug", buffer = true }))
        
        vim.keymap.set("n", "<leader>rdr", function()
          vim.cmd.RustLsp("runnables")
        end, vim.tbl_extend("force", opts, { desc = "Runnables", buffer = true }))
      end
    end
  })
  
  -- Crates.nvim keymaps (available in Cargo.toml files)
  vim.api.nvim_create_autocmd("FileType", {
    pattern = "toml",
    callback = function()
      local has_crates = pcall(require, "crates")
      if has_crates and vim.fn.expand("%:t") == "Cargo.toml" then
        vim.keymap.set("n", "<leader>ct", function()
          require("crates").toggle()
        end, vim.tbl_extend("force", opts, { desc = "Toggle crates", buffer = true }))
        
        vim.keymap.set("n", "<leader>cr", function()
          require("crates").reload()
        end, vim.tbl_extend("force", opts, { desc = "Reload crates", buffer = true }))
        
        vim.keymap.set("n", "<leader>cv", function()
          require("crates").show_versions_popup()
        end, vim.tbl_extend("force", opts, { desc = "Show versions", buffer = true }))
        
        vim.keymap.set("n", "<leader>cf", function()
          require("crates").show_features_popup()
        end, vim.tbl_extend("force", opts, { desc = "Show features", buffer = true }))
        
        vim.keymap.set("n", "<leader>cd", function()
          require("crates").show_dependencies_popup()
        end, vim.tbl_extend("force", opts, { desc = "Show dependencies", buffer = true }))
        
        vim.keymap.set("n", "<leader>cu", function()
          require("crates").update_crate()
        end, vim.tbl_extend("force", opts, { desc = "Update crate", buffer = true }))
        
        vim.keymap.set("v", "<leader>cu", function()
          require("crates").update_crates()
        end, vim.tbl_extend("force", opts, { desc = "Update crates", buffer = true }))
        
        vim.keymap.set("n", "<leader>ca", function()
          require("crates").update_all_crates()
        end, vim.tbl_extend("force", opts, { desc = "Update all crates", buffer = true }))
        
        vim.keymap.set("n", "<leader>cU", function()
          require("crates").upgrade_crate()
        end, vim.tbl_extend("force", opts, { desc = "Upgrade crate", buffer = true }))
        
        vim.keymap.set("v", "<leader>cU", function()
          require("crates").upgrade_crates()
        end, vim.tbl_extend("force", opts, { desc = "Upgrade crates", buffer = true }))
        
        vim.keymap.set("n", "<leader>cA", function()
          require("crates").upgrade_all_crates()
        end, vim.tbl_extend("force", opts, { desc = "Upgrade all crates", buffer = true }))
        
        vim.keymap.set("n", "<leader>cH", function()
          require("crates").open_homepage()
        end, vim.tbl_extend("force", opts, { desc = "Open homepage", buffer = true }))
        
        vim.keymap.set("n", "<leader>cR", function()
          require("crates").open_repository()
        end, vim.tbl_extend("force", opts, { desc = "Open repository", buffer = true }))
        
        vim.keymap.set("n", "<leader>cD", function()
          require("crates").open_documentation()
        end, vim.tbl_extend("force", opts, { desc = "Open documentation", buffer = true }))
        
        vim.keymap.set("n", "<leader>cC", function()
          require("crates").open_crates_io()
        end, vim.tbl_extend("force", opts, { desc = "Open crates.io", buffer = true }))
      end
    end
  })
end

return M