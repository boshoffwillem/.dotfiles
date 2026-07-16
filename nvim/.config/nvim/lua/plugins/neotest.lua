return {
  "nvim-neotest/neotest",
  dependencies = {
    "nvim-neotest/nvim-nio",
    "antoinemadec/FixCursorHold.nvim",
    "nsidorenco/neotest-vstest",
  },
  config = function()
    require("neotest").setup({
      adapters = {
        require("neotest-vstest"),
      },
    })

    local debugTest = function()
      require("neotest").run.run({ strategy = "dap" })
    end

    vim.keymap.set("n", "<leader>tr", require("neotest").run.run, { desc = "[T]est [D]ebug" })
    vim.keymap.set("n", "<leader>td", debugTest, { desc = "[T]est [D]ebug" })

    -- local map = function(keys, func, desc, mode)
    --   mode = mode or "n"
    --   vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = "LSP: " .. desc })
    -- end
    --
    -- map("<leader>tr", require("neotest").run.run(), "[T]est [R]un")
    -- map("<leader>td", require("neotest").run.run({ strategy = "dap" }), "[T]est [D]ebug")
  end,
}
