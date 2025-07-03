require("conform").setup({
  formatters_by_ft = {
    cs = { "csharpier" },
    javascript = { "prettierd", "prettier", stop_after_first = true },
    json = { "prettierd", "prettier", stop_after_first = true },
    lua = { "stylua" },
    python = { "isort", "black" },
    rust = { "rustfmt", lsp_format = "fallback" },
    terraform = { "terraformfmt" },
    typescript = { "prettierd", "prettier", stop_after_first = true },
  },
  format_on_save = {
    -- These options will be passed to conform.format()
    timeout_ms = 500,
    lsp_format = "fallback",
  },
})

-- Provides the Format, FormatWrite, FormatLock, and FormatWriteLock commands
-- require("formatter").setup({
--   -- Enable or disable logging
--   logging = true,
--   -- Set the log level
--   log_level = vim.log.levels.WARN,
--   -- All formatter configurations are opt-in
--   filetype = {
--     -- Formatter configurations for filetype "lua" go here
--     -- and will be executed in order
--     cs = {
--       require("formatter.filetypes.cs").csharpier,
--     },
--     html = {
--       require("formatter.filetypes.html").prettier,
--     },
--     lua = {
--       require("formatter.filetypes.lua").stylua,
--     },
--     javascript = {
--       require("formatter.filetypes.javascript").prettier,
--     },
--     json = {
--       require("formatter.filetypes.json").prettier,
--     },
--     proto = {
--       require("formatter.filetypes.proto").buf_format,
--     },
--     rust = {
--       require("formatter.filetypes.rust").rustfmt,
--     },
--     terraform = {
--       require("formatter.filetypes.terraform").terraformfmt,
--     },
--     typescript = {
--       require("formatter.filetypes.typescript").prettier,
--     },
--     vue = {
--       require("formatter.filetypes.vue").prettier,
--     },
--     xml = {
--       require("formatter.filetypes.xml").xmlformat,
--     },
--     yaml = {
--       require("formatter.filetypes.yaml").prettier,
--     },
--     ["*"] = {
--       require("formatter.filetypes.any").remove_trailing_whitespace,
--     },
--   },
-- })
--
-- local opts = { noremap = true, silent = true }
-- local keymap = vim.api.nvim_set_keymap
-- keymap("n", "<leader>f", ":FormatWrite<CR>", opts)
-- local group = vim.api.nvim_create_augroup("FormatAutogroup", { clear = true })
-- vim.api.nvim_create_autocmd("BufWritePost", { group = group, command = "FormatWrite" })
