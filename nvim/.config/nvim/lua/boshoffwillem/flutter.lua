require("flutter-tools").setup {
  ui = {
    border = "rounded",
    notification_style = "native",
  },
  decorations = {
    statusline = {
      app_version = false,
      device = true,
      project_config = false,
    }
  },
  debugger = {
    enabled = true,
    run_via_dap = true,
    register_configurations = function(paths)
      local dap = require("dap")
      dap.adapters.dart = {
        type = "executable",
        command = paths.flutter_bin,
        args = { "debug-adapter" },
      }
      dap.configurations.dart = {}
      require("dap.ext.vscode").load_launchjs()
    end,
  },
  flutter_path = "/Users/boshoffwillem/development/flutter/bin/flutter",
  flutter_lookup_cmd = nil,
  root_patterns = { ".git", "pubspec.yaml" },
  fvm = false,
  widget_guides = {
    enabled = false,
  },
  closing_tags = {
    highlight = "ErrorMsg",
    prefix = ">",
    enabled = true
  },
  dev_log = {
    enabled = true,
    notify_errors = false,
    open_cmd = "tabedit",
  },
  dev_tools = {
    autostart = false,
    auto_open_browser = false,
  },
  outline = {
    open_cmd = "30vnew",
    auto_open = false
  },
  lsp = {
    color = {
      enabled = false,
      background = false,
      background_color = nil,
      foreground = false,
      virtual_text = true,
      virtual_text_str = "■",
    },
    init_options = {
      onlyAnalyzeProjectsWithOpenFiles = true,
      suggestFromUnimportedLibraries = true,
      closingLabels = true,
      outline = true,
      flutterOutline = true,
    },
    on_attach = function(client, bufnr)
      -- Set up universal LSP keybindings first
      require("boshoffwillem.universal-keybinds").setup_lsp_keybinds(client, bufnr)
      
      local nmap = function(keys, func, desc)
        if desc then
          desc = "LSP: " .. desc
        end
        vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc })
      end

      -- Only Flutter/Dart-specific keybindings should be added here
        nmap("<space>lr", vim.lsp.buf.rename, "")
        nmap("<space>la", vim.lsp.buf.code_action, "")
        nmap("<space>l=", function()
          vim.lsp.buf.format({ async = true })
        end, "")
        nmap("<leader>pt", require("telescope.builtin").lsp_dynamic_workspace_symbols, "")

      vim.api.nvim_buf_create_user_command(bufnr, "Format", function(_)
        vim.lsp.buf.format()
      end, { desc = "Format current buffer with LSP" })
    end,
    capabilities = require("cmp_nvim_lsp").default_capabilities(),
    settings = {
      showTodos = true,
      completeFunctionCalls = true,
      renameFilesWithClasses = "prompt",
      enableSnippets = true,
      updateImportsOnRename = true,
    }
  }
}
