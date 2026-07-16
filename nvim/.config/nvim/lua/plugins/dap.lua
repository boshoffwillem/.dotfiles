return {
  "mfussenegger/nvim-dap",
  config = function()
    local dap = require("dap")
    local dap_dotnet = require("nvim-dap-dotnet")
    dap.adapters["local-lua"] = {
      type = "executable",
      command = "node",
      args = {
        vim.fn.stdpath("data")
          .. "/mason/packages/local-lua-debugger-vscode/ocal-lua-debugger-vscode/extension/extension/debugAdapter.js",
      },
      enrich_config = function(config, on_config)
        if not config["extensionPath"] then
          local c = vim.deepcopy(config)
          -- 💀 If this is missing or wrong you'll see
          -- "module 'lldebugger' not found" errors in the dap-repl when trying to launch a debug session
          c.extensionPath = "~/code/local-lua-debugger-vscode/"
          on_config(c)
        else
          on_config(config)
        end
      end,
    }

    local netcoredbg_adapter = {
      type = "executable",
      command = vim.fn.stdpath("data") .. "/mason/packages/netcoredbg/netcoredbg",
      args = { "--interpreter=vscode" },
    }

    dap.adapters.netcoredbg = netcoredbg_adapter -- needed for normal debugging
    dap.adapters.coreclr = netcoredbg_adapter -- needed for unit test debugging

    dap.configurations.cs = {
      {
        type = "coreclr",
        name = "launch - netcoredbg",
        request = "launch",
        program = function()
          return dap_dotnet.build_artifact_dll_path()
        end,
      },
    }
    dap.configurations.lua = {
      {
        name = "Current file",
        type = "local-lua",
        request = "launch",
        cwd = "${workspaceFolder}",
        program = {
          lua = "lua5.1",
          file = "${file}",
        },
        args = {},
      },
    }

    vim.keymap.set("n", "<leader>ds", dap.continue, { desc = "[D]ebug Continue/Start" })
    vim.keymap.set("n", "<F5>", dap.continue, { desc = "[D]ebug Continue/Start" })
    vim.keymap.set("n", "<leader>db", dap.toggle_breakpoint, { desc = "[D]ebug Toggle [B]reakpoint" })
    vim.keymap.set("n", "<F10>", dap.step_over, { desc = "DAP: Step over" })
    vim.keymap.set("n", "<F11>", dap.step_into, { desc = "DAP: Step into" })
    vim.keymap.set("n", "<F8>", dap.step_out, { desc = "DAP: Step out" })
    vim.keymap.set("n", "<leader>dr", dap.repl.open, { desc = "[D]ebug [R]EPL Open" })
    vim.keymap.set("n", "<leader>dl", dap.run_last, { desc = "[D]ebug Run [L]ast" })
  end,
}
