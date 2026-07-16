return {
  "rcarriga/nvim-dap-ui",
  dependencies = { "mfussenegger/nvim-dap", "nvim-neotest/nvim-nio" },
  config = function()
    require("lazydev").setup({
      library = { "nvim-dap-ui" },
    })
    require("dapui").setup()

    vim.keymap.set("n", "<leader>do", require("dapui").open, { desc = "[D]ebug UI [O]open" })
    vim.keymap.set("n", "<leader>dc", require("dapui").close, { desc = "[D]ebug UI [C]lose" })
    vim.keymap.set("n", "<leader>dt", require("dapui").toggle, { desc = "[D]ebug UI [T]oggle" })
  end,
}
