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
    on_attach = function(client, bufnr)
      local on_attach = function(client, bufnr)
        local nmap = function(keys, func, desc)
          if desc then
            desc = "LSP: " .. desc
          end
          vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc })
        end

        nmap("gD", vim.lsp.buf.declaration, "[G]oto [D]eclaration")
        nmap("<leader>D", vim.lsp.buf.type_definition, "[G]oto [T]ype [D]efinition")
        nmap("gd", vim.lsp.buf.definition, "[G]oto [D]efinition")
        nmap("gr", vim.lsp.buf.references, "")
        nmap("gi", vim.lsp.buf.implementation, "")
        nmap("K", vim.lsp.buf.hover, "")
        nmap("<C-k>", vim.lsp.buf.signature_help, "")
        nmap("<space>wa", vim.lsp.buf.add_workspace_folder, "")
        nmap("<space>wr", vim.lsp.buf.remove_workspace_folder, "")
        nmap("<space>wl", function()
          print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
        end, "")
        nmap("<space>D", vim.lsp.buf.type_definition, "")
        nmap("<space>lr", vim.lsp.buf.rename, "")
        nmap("<space>la", vim.lsp.buf.code_action, "")
        nmap("<space>l=", function()
          vim.lsp.buf.format({ async = true })
        end, "")
        nmap("<leader>pt", require("telescope.builtin").lsp_dynamic_workspace_symbols, "")

        vim.api.nvim_buf_create_user_command(bufnr, "Format", function(_)
          vim.lsp.buf.format()
        end, { desc = "Format current buffer with LSP" })
      end
      on_attach(client, bufnr)
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
