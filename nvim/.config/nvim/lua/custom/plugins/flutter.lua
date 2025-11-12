return {
  {
    "akinsho/flutter-tools.nvim",
    ft = { "dart" },
    dependencies = {
      "nvim-lua/plenary.nvim",
      "stevearc/dressing.nvim", -- optional for vim.ui.select
    },
    config = function()
      require("flutter-tools").setup({
        ui = {
          -- The border type to use for all floating windows if not specified
          border = "rounded",
          notification_style = "native",
        },
        decorations = {
          statusline = {
            -- Set to true to be able to use the 'flutter_tools_decorations.app_version' in your statusline
            app_version = true,
            device = true,
            project_config = false,
          },
        },
        debugger = {
          -- Integrate with nvim dap
          enabled = true,
          run_via_dap = true,
          -- If nvim-dap is installed, set to true to use nvim-dap for debugging
          register_configurations = function(paths)
            require("dap").configurations.dart = {
              {
                type = "dart",
                request = "launch",
                name = "Launch Flutter Program",
                -- The nvim-dap plugin populates this variable with the filename of the current buffer
                program = "${file}",
                -- The nvim-dap plugin populates this variable with the editor's current working directory
                cwd = "${workspaceFolder}",
                -- This gets forwarded to the Flutter CLI tool, substitute `linux` for whatever device you wish to launch
                args = { "--flavor", "dev" },
              },
            }
          end,
        },
        fvm = false, -- Set to true if you are using FVM
        widget_guides = {
          enabled = true,
        },
        closing_tags = {
          highlight = "Comment", -- Highlight for the closing tags
          prefix = "// ", -- Character to use for close tags e.g. > Widget
          enabled = true, -- Set to false to disable
        },
        dev_log = {
          enabled = true,
          notify_errors = false, -- If there is an error whilst running then notify the user
          open_cmd = "tabedit", -- Command to use to open the log buffer
        },
        dev_tools = {
          autostart = false, -- Autostart devtools server if not detected
          auto_open_browser = false, -- Automatically opens devtools in the browser
        },
        outline = {
          open_cmd = "30vnew", -- Command to use to open the outline buffer
          auto_open = false, -- If true this will open the outline automatically when it is first populated
        },
        lsp = {
          color = {
            enabled = true, -- Whether or not to highlight color variables at all
            background = false, -- Highlight the background
            background_color = nil, -- Required if background is set to true
            foreground = false, -- Highlight the foreground
            virtual_text = true, -- Show the highlight using virtual text
            virtual_text_str = "■", -- The virtual text character to highlight
          },
          on_attach = function(client, bufnr)
            -- Custom on_attach logic here if needed
          end,
          capabilities = function()
            local capabilities = vim.lsp.protocol.make_client_capabilities()
            -- Add blink.cmp capabilities
            capabilities = require("blink.cmp").get_lsp_capabilities(capabilities)
            return capabilities
          end,
          -- See the link below for details on each option:
          -- https://github.com/dart-lang/sdk/blob/master/pkg/analysis_server/tool/lsp_spec/README.md#client-workspace-configuration
          settings = {
            showTodos = true,
            completeFunctionCalls = true,
            analysisExcludedFolders = {
              vim.fn.expand("$HOME/.pub-cache"),
              vim.fn.expand("$HOME/flutter/.pub-cache"),
            },
            renameFilesWithClasses = "prompt", -- "always"
            enableSnippets = true,
            updateImportsOnRename = true, -- Whether to update imports when renaming files
          },
        },
      })

      -- Telescope integration (optional)
      require("telescope").load_extension("flutter")

      -- Keymaps for Flutter commands
      local map = vim.keymap.set
      local opts = { noremap = true, silent = true }

      -- Flutter commands
      map("n", "<leader>Fs", "<cmd>FlutterRun<cr>", vim.tbl_extend("force", opts, { desc = "[F]lutter [S]tart/Run" }))
      map("n", "<leader>Fq", "<cmd>FlutterQuit<cr>", vim.tbl_extend("force", opts, { desc = "[F]lutter [Q]uit" }))
      map(
        "n",
        "<leader>Fr",
        "<cmd>FlutterReload<cr>",
        vim.tbl_extend("force", opts, { desc = "[F]lutter [R]eload (Hot Reload)" })
      )
      map(
        "n",
        "<leader>FR",
        "<cmd>FlutterRestart<cr>",
        vim.tbl_extend("force", opts, { desc = "[F]lutter [R]estart (Hot Restart)" })
      )
      map(
        "n",
        "<leader>Fd",
        "<cmd>FlutterDevices<cr>",
        vim.tbl_extend("force", opts, { desc = "[F]lutter [D]evices" })
      )
      map(
        "n",
        "<leader>Fe",
        "<cmd>FlutterEmulators<cr>",
        vim.tbl_extend("force", opts, { desc = "[F]lutter [E]mulators" })
      )
      map(
        "n",
        "<leader>Fo",
        "<cmd>FlutterOutlineToggle<cr>",
        vim.tbl_extend("force", opts, { desc = "[F]lutter [O]utline Toggle" })
      )
      map("n", "<leader>Ft", "<cmd>FlutterDevTools<cr>", vim.tbl_extend("force", opts, { desc = "[F]lutter Dev[T]ools" }))
      map("n", "<leader>Fc", "<cmd>FlutterCopyProfilerUrl<cr>", vim.tbl_extend("force", opts, { desc = "[F]lutter [C]opy Profiler URL" }))
      map(
        "n",
        "<leader>Fl",
        "<cmd>FlutterLspRestart<cr>",
        vim.tbl_extend("force", opts, { desc = "[F]lutter [L]sp Restart" })
      )
      map(
        "n",
        "<leader>Fp",
        "<cmd>FlutterPubGet<cr>",
        vim.tbl_extend("force", opts, { desc = "[F]lutter [P]ub Get" })
      )

      -- Telescope Flutter commands
      map(
        "n",
        "<leader>sF",
        "<cmd>Telescope flutter commands<cr>",
        vim.tbl_extend("force", opts, { desc = "[S]earch [F]lutter Commands" })
      )
    end,
  },
}
