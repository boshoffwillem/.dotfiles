return {
  -- cargo install --locked tree-sitter-cli
  "nvim-treesitter/nvim-treesitter",
  lazy = false,
  build = "TSUpdate",
  config = function()
    local filetypes = {
      "bash",
      "c",
      "c_sharp",
      "diff",
      "elixir",
      "heex",
      "html",
      "javascript",
      "json",
      "lua",
      "luadoc",
      "markdown",
      "markdown_inline",
      "python",
      "query",
      "typescript",
      "vim",
      "vimdoc",
    }

    require("nvim-treesitter.configs").setup({
      ensure_installed = filetypes,
      highlight = { enable = true },
      indent = { enable = true },
    })

    vim.api.nvim_create_autocmd("FileType", {
      pattern = filetypes,
      callback = function()
        vim.treesitter.start()
        vim.wo[0][0].foldexpr = "v:lua.vim.treesitter.foldexpr()"
        vim.wo[0][0].foldmethod = "expr"
        vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
      end,
    })
  end,
}
