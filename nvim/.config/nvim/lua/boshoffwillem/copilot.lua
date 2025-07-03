-- require("copilot").setup({
vim.g.copilot_assume_mapped = true
vim.api.nvim_set_keymap("i", "<C-y>", 'copilot#Accept("<CR>")', { silent = true, expr = true })
vim.g.copilot_no_tab_map = true

require("CopilotChat").setup {
  -- See Configuration section for options
}
