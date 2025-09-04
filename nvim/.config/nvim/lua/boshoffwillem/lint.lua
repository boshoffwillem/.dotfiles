require("lint").linters_by_ft = {
  dockerfile = { "hadolint" },
  editorconfig = { "editorconfig-checker" },
  java = { "checkstyle" },
  json = { "jsonlint" },
  kotlin = { "ktlint" },
  proto = { "buf_lint" },
  swift = { "swiftlint" },
  yaml = { "yamllint" },
}

vim.api.nvim_create_autocmd({ "BufWritePost" }, {
  callback = function()
    require("lint").try_lint()
  end,
})
