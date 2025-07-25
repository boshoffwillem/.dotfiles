-- Install packer
local install_path = vim.fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
local is_bootstrap = false
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  is_bootstrap = true
  vim.fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
  vim.cmd([[packadd packer.nvim]])
end

require("packer").startup(function(use)
  -- Package manager
  use("wbthomason/packer.nvim")

  -- floating terminal
  use({
    "akinsho/toggleterm.nvim",
    tag = "*",
    config = function()
      require("toggleterm").setup()
    end,
  })

  -- LSP Support
  use({ "neovim/nvim-lspconfig" })
  use({ "mfussenegger/nvim-dap" })
  use({ "mfussenegger/nvim-lint" })
  use({ "rcarriga/nvim-dap-ui", requires = { "mfussenegger/nvim-dap", "nvim-neotest/neotest" } })
  use({ "theHamsta/nvim-dap-virtual-text" })
  use({
    "nvim-neotest/neotest",
    requires = {
      "nvim-neotest/nvim-nio",
      "nvim-lua/plenary.nvim",
      "antoinemadec/FixCursorHold.nvim",
      "nvim-treesitter/nvim-treesitter",
      "Issafalcon/neotest-dotnet",
    },
  })
  use({ "williamboman/mason.nvim" })
  use({ "williamboman/mason-lspconfig.nvim" })
  use({ "mhartington/formatter.nvim" })
  use("stevearc/conform.nvim")
  use({ "lewis6991/hover.nvim" })

  -- Autocompletion
  use("hrsh7th/nvim-cmp")
  use("hrsh7th/cmp-buffer")
  use("hrsh7th/cmp-path")
  use("saadparwaiz1/cmp_luasnip")
  use("hrsh7th/cmp-nvim-lsp")
  use("hrsh7th/cmp-nvim-lua")

  -- Snippets
  use("L3MON4D3/LuaSnip")
  use("rafamadriz/friendly-snippets")

  -- Useful status updates for LSP
  use({ "j-hui/fidget.nvim" })

  -- JSON and YAML schemas
  use("b0o/schemastore.nvim")

  -- Additional lua configuration makes nvim stuff amazing
  use("folke/neodev.nvim")

  local function tabnine_build_path()
    -- Replace vim.uv with vim.loop if using NVIM 0.9.0 or below
    if vim.uv.os_uname().sysname == "Windows_NT" then
      return "pwsh.exe -file .\\dl_binaries.ps1"
    else
      return "./dl_binaries.sh"
    end
  end

  use({ "codota/tabnine-nvim", build = tabnine_build_path() })

  use("nvim-treesitter/playground")

  -- Highlight, edit, and navigate code
  use({
    "nvim-treesitter/nvim-treesitter",
    run = function()
      pcall(require("nvim-treesitter.install").update({ with_sync = true }))
    end,
  })

  -- Additional text objects via treesitter
  use({
    "nvim-treesitter/nvim-treesitter-textobjects",
    after = "nvim-treesitter",
  })

  -- Git related plugins
  use("tpope/vim-fugitive")
  use("tpope/vim-rhubarb")
  use("lewis6991/gitsigns.nvim")

  use("Mofiqul/vscode.nvim")
  use("ishan9299/modus-theme-vim")
  use("kyazdani42/nvim-web-devicons")
  use("nvim-lualine/lualine.nvim") -- Fancier statusline
  -- use({ "lukas-reineke/indent-blankline.nvim", main = "ibl", opts = {} }) -- Add indentation guides even on blank lines
  use("numToStr/Comment.nvim") -- "gc" to comment visual regions/lines
  use("tpope/vim-sleuth") -- Detect tabstop and shiftwidth automatically
  use("mbbill/undotree")

  -- Fuzzy Finder (files, lsp, etc)
  use({ "nvim-telescope/telescope.nvim", requires = { "nvim-lua/plenary.nvim" } })
  use("ThePrimeagen/harpoon")
  use({ "nvim-telescope/telescope-project.nvim" })

  -- Fuzzy Finder Algorithm which requires local dependencies to be built. Only load if `make` is available
  -- use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make", cond = vim.fn.executable("make") == 1 })

  -- Add custom plugins to packer from ~/.config/nvim/lua/custom/plugins.lua
  local has_plugins, plugins = pcall(require, "custom.plugins")
  if has_plugins then
    plugins(use)
  end

  if is_bootstrap then
    require("packer").sync()
  end
end)

-- When we are bootstrapping a configuration, it doesn't
-- make sense to execute the rest of the init.lua.
--
-- You'll need to restart nvim, and then it will work.
if is_bootstrap then
  print("==================================")
  print("    Plugins are being installed")
  print("    Wait until Packer completes,")
  print("       then restart nvim")
  print("==================================")
  return
end

-- Automatically source and re-compile packer whenever you save this init.lua
local packer_group = vim.api.nvim_create_augroup("Packer", { clear = true })
vim.api.nvim_create_autocmd("BufWritePost", {
  command = "source <afile> | PackerCompile",
  group = packer_group,
  pattern = vim.fn.expand("$MYVIMRC"),
})
