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
      dap.configurations.dart = {
        {
          type = "dart",
          request = "launch",
          name = "Launch Flutter",
          program = "${workspaceFolder}/lib/main.dart",
          cwd = "${workspaceFolder}",
          toolArgs = { "--device-id", "flutter-tester" }, -- Adjust device as needed
        },
      }
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
    cmd = { "dart", "language-server", "--client-id", "neovim" },
    root_dir = require("lspconfig.util").root_pattern("pubspec.yaml"),
    color = {
      enabled = true,
    },
    init_options = {
      onlyAnalyzeProjectsWithOpenFiles = true,
      suggestFromUnimportedLibraries = true,
      closingLabels = true,
      outline = true,
      flutterOutline = true,
    },
    settings = {
      showTodos = true,
      completeFunctionCalls = true,
      renameFilesWithClasses = "prompt",
      enableSnippets = true,
      updateImportsOnRename = true,
    }
  }
}
