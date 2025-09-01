local M = {}

function M.setup()
  local has_neotest, neotest = pcall(require, "neotest")
  if not has_neotest then
    vim.notify("neotest not found", vim.log.levels.WARN)
    return
  end

  neotest.setup({
    adapters = {
      require("neotest-dotnet")({
        dap = {
          adapter_name = "coreclr",
          args = { justMyCode = false },
        },
        discovery_root = "solution",
        custom_attributes = {
          xunit = { "Fact", "Theory" },
          nunit = { "Test", "TestCase", "TestCaseSource" },
          mstest = { "TestMethod" },
        },
        dotnet_additional_args = {
          "--verbosity=normal",
          "--logger=console;verbosity=detailed",
        },
      }),
    },
    discovery = {
      enabled = true,
      concurrent = 2,
    },
    running = {
      concurrent = true,
    },
    summary = {
      enabled = true,
      expand_errors = true,
      follow = true,
      mappings = {
        attach = "a",
        clear_marked = "M",
        clear_target = "T",
        debug = "d",
        debug_marked = "D",
        expand = { "<CR>", "<2-LeftMouse>" },
        expand_all = "e",
        jumpto = "i",
        mark = "m",
        next_failed = "J",
        output = "o",
        prev_failed = "K",
        run = "r",
        run_marked = "R",
        short = "O",
        stop = "u",
        target = "t",
        watch = "w",
      },
    },
    output = {
      enabled = true,
      open_on_run = "short",
    },
    output_panel = {
      enabled = true,
      open = "botright split | resize 15",
    },
    status = {
      enabled = true,
      signs = true,
      virtual_text = false,
    },
    diagnostic = {
      enabled = true,
      severity = vim.diagnostic.severity.ERROR,
    },
    log_level = vim.log.levels.WARN,
    icons = {
      child_indent = "│",
      child_prefix = "├",
      collapsed = "─",
      expanded = "╮",
      failed = "✘",
      final_child_indent = " ",
      final_child_prefix = "╰",
      non_collapsible = "─",
      passed = "✓",
      running = "●",
      running_animated = { "/", "|", "\\", "-", "/", "|", "\\", "-" },
      skipped = "○",
      unknown = "?"
    },
  })
end

function M.setup_keymaps()
  local opts = { noremap = true, silent = true }
  
  -- Universal test keybindings are handled by universal-keybinds.lua
  -- Keep neotest-specific keybindings with different prefixes
  vim.keymap.set("n", "<leader>tn", function()
    require("neotest").run.run()
  end, vim.tbl_extend("force", opts, { desc = "[T]est [N]earest (neotest)" }))
  
  vim.keymap.set("n", "<leader>tf", function()
    require("neotest").run.run(vim.fn.expand("%"))
  end, vim.tbl_extend("force", opts, { desc = "[T]est [F]ile (neotest)" }))
  
  vim.keymap.set("n", "<leader>tA", function()
    require("neotest").run.run(vim.fn.getcwd())
  end, vim.tbl_extend("force", opts, { desc = "[T]est [A]ll (neotest specific)" }))
  
  vim.keymap.set("n", "<leader>td", function()
    require("neotest").run.run({ strategy = "dap" })
  end, vim.tbl_extend("force", opts, { desc = "Debug nearest test" }))
  
  vim.keymap.set("n", "<leader>ts", function()
    require("neotest").run.stop()
  end, vim.tbl_extend("force", opts, { desc = "Stop tests" }))
  
  vim.keymap.set("n", "<leader>to", function()
    require("neotest").output.open({ enter = true })
  end, vim.tbl_extend("force", opts, { desc = "Show test output" }))
  
  vim.keymap.set("n", "<leader>tO", function()
    require("neotest").output_panel.toggle()
  end, vim.tbl_extend("force", opts, { desc = "Toggle output panel" }))
  
  vim.keymap.set("n", "<leader>tS", function()
    require("neotest").summary.toggle()
  end, vim.tbl_extend("force", opts, { desc = "Toggle test summary" }))
  
  vim.keymap.set("n", "]t", function()
    require("neotest").jump.next({ status = "failed" })
  end, vim.tbl_extend("force", opts, { desc = "Next failed test" }))
  
  vim.keymap.set("n", "[t", function()
    require("neotest").jump.prev({ status = "failed" })
  end, vim.tbl_extend("force", opts, { desc = "Previous failed test" }))
  
  vim.keymap.set("n", "<leader>tw", function()
    require("neotest").watch.toggle()
  end, vim.tbl_extend("force", opts, { desc = "Toggle watch mode" }))
end

return M
