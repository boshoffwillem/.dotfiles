require("lint").linters_by_ft = {
	dockerfile = { "hadolint" },
	editorconfig = { "editorconfig-checker" },
	json = { "jsonlint" },
	proto = { "buf_lint" },
	terraform = { "trivy" },
	yaml = { "yamllint" },
}

vim.api.nvim_create_autocmd({ "BufWritePost" }, {
	callback = function()
		require("lint").try_lint()
	end,
})
