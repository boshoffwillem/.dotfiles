-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end

vim.opt.rtp:prepend(lazypath)

-- require("lazy").setup("plugins", {
--   change_detection = { enabled = true, notify = false }
-- })

-- Setup lazy.nvim with all plugins
require("lazy").setup({
  -- Theme plugins
  "ellisonleao/gruvbox.nvim",
  "Mofiqul/vscode.nvim",
  "ishan9299/modus-theme-vim",

  -- C#/.NET specific plugins
  "Issafalcon/lsp-overloads.nvim",     -- Show method overloads
  "Hoffs/omnisharp-extended-lsp.nvim", -- Extended omnisharp features
  {
    "iabdelkareem/csharp.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "Tastyep/structlog.nvim",
    },
  },
  {
    "GustavEikaas/easy-dotnet.nvim",
    dependencies = { "nvim-lua/plenary.nvim", "nvim-telescope/telescope.nvim" },
    config = function()
      require("easy-dotnet").setup()
    end,
  },

  -- Terminal
  {
    "akinsho/toggleterm.nvim",
    version = "*",
    config = function()
      require("toggleterm").setup()
    end,
  },

  -- Flutter
  {
    "nvim-flutter/flutter-tools.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "stevearc/dressing.nvim", -- optional for vim.ui.select
    },
    config = true,
  },

  -- Java/Kotlin/Android development
  "mfussenegger/nvim-jdtls", -- Enhanced Java LSP support for Android

  -- Swift/iOS development
  "keith/swift.vim", -- Swift syntax and indentation
  {
    "wojciech-kulik/xcodebuild.nvim",
    dependencies = {
      "nvim-telescope/telescope.nvim",
      "MunifTanjim/nui.nvim",
    },
    config = function()
      require("xcodebuild").setup({
        -- Configuration will be added if needed
      })
    end,
  },

  -- Angular development
  "joeveiga/ng.nvim", -- Angular CLI integration

  -- Vue.js development
  "posva/vim-vue",             -- Vue syntax highlighting
  "leafOfTree/vim-vue-plugin", -- Enhanced Vue support

  -- TypeScript/JavaScript enhanced tools
  {
    "pmizio/typescript-tools.nvim",
    dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
    config = function()
      require("typescript-tools").setup({})
    end,
  },

  -- Go development
  {
    "ray-x/go.nvim",
    dependencies = {
      "ray-x/guihua.lua", -- Go.nvim dependency
      "neovim/nvim-lspconfig",
      "nvim-treesitter/nvim-treesitter",
    },
    config = function()
      require("go").setup()
    end,
    event = { "CmdlineEnter" },
    ft = { "go", "gomod" },
    build = ':lua require("go.install").update_all_sync()', -- if you need to install/update all binaries
  },

  -- Rust development
  {
    "mrcjkb/rustaceanvim",
    version = "^4",
    ft = { "rust" },
  },
  {
    "saecki/crates.nvim",
    tag = "stable",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("crates").setup()
    end,
  },

  -- LSP Support
  {
    "neovim/nvim-lspconfig",
    -- Mason
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "hrsh7th/nvim-cmp",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "saadparwaiz1/cmp_luasnip",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-nvim-lua",
      "L3MON4D3/LuaSnip",
    },
  },
  "mfussenegger/nvim-lint",
  {
    "mfussenegger/nvim-dap",
    dependencies = { "rcarriga/nvim-dap-ui", "nvim-neotest/nvim-nio", "leoluz/nvim-dap-go", "theHamsta/nvim-dap-virtual-text" },
  },

  -- Testing
  {
    "nvim-neotest/neotest",
    dependencies = {
      "nvim-neotest/nvim-nio",
      "nvim-lua/plenary.nvim",
      "antoinemadec/FixCursorHold.nvim",
      "nvim-treesitter/nvim-treesitter",
      "Issafalcon/neotest-dotnet",
      "nvim-neotest/neotest-jest",
      "marilari88/neotest-vitest",
      "nvim-neotest/neotest-go",
      "rouge8/neotest-rust",
    },
  },

  -- Formatting
  "mhartington/formatter.nvim",
  "stevearc/conform.nvim",
  "lewis6991/hover.nvim",

  -- Snippets
  "rafamadriz/friendly-snippets",

  -- Useful status updates for LSP
  "j-hui/fidget.nvim",

  -- JSON and YAML schemas
  "b0o/schemastore.nvim",

  -- Additional lua configuration
  "folke/neodev.nvim",

  -- Treesitter
  "nvim-treesitter/playground",
  {
    "nvim-treesitter/nvim-treesitter",
    build = ":TSUpdate",
  },
  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    dependencies = "nvim-treesitter/nvim-treesitter",
  },

  -- Git related plugins
  "tpope/vim-fugitive",
  "tpope/vim-rhubarb",
  "lewis6991/gitsigns.nvim",

  -- UI plugins
  "kyazdani42/nvim-web-devicons",
  "nvim-lualine/lualine.nvim",
  "numToStr/Comment.nvim",
  "tpope/vim-sleuth", -- Detect tabstop and shiftwidth automatically
  "mbbill/undotree",

  -- File explorer
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "kyazdani42/nvim-web-devicons",
      "MunifTanjim/nui.nvim",
    },
  },

  -- Fuzzy Finder (files, lsp, etc)
  {
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
  },
  "ThePrimeagen/harpoon",
  "nvim-telescope/telescope-project.nvim",
}, {
  -- Lazy.nvim configuration options
  defaults = {
    lazy = false, -- Don't lazy-load by default to match Packer behavior
  },
  ui = {
    border = "rounded",
  },
  checker = {
    enabled = true, -- Automatically check for plugin updates
    notify = false, -- Don't notify on startup
  },
  performance = {
    rtp = {
      disabled_plugins = {
        "gzip",
        "matchit",
        "matchparen",
        "netrwPlugin",
        "tarPlugin",
        "tohtml",
        "tutor",
        "zipPlugin",
      },
    },
  },
})

-- Load custom plugins if they exist (maintaining compatibility)
local has_custom, custom = pcall(require, "custom.plugins")
if has_custom and type(custom) == "table" then
  -- If custom.plugins returns a table of plugin specs, add them
  require("lazy").setup(custom, {
    root = vim.fn.stdpath("data") .. "/lazy",
  })
end
