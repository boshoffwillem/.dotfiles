-- [[ Setting options ]]
-- See `:help vim.o`

-- " Add a new named configuration
-- call cyclist#add_listchar_option_set('limited', {
--         \ 'eol': '↲',
--         \ 'tab': '» ',
--         \ 'trail': '·',
--         \ 'extends': '<',
--         \ 'precedes': '>',    
--         \ 'conceal': '┊',
--         \ 'nbsp': '␣',
--         \ })
vim.opt.listchars:append({ eol = "↲" })
vim.opt.listchars:append({ tab = "» " })
vim.opt.listchars:append({ trail = "." })
vim.opt.list = true

-- Set highlight on search
vim.o.hlsearch = false
vim.o.incsearch = true

-- Make line numbers default
vim.wo.number = true

-- Enable mouse mode
vim.o.mouse = 'a'

-- Enable break indent
vim.o.breakindent = true

-- Save undo history
vim.o.undofile = true
vim.o.undodir = os.getenv("HOME") .. "/.vim/undodir"
vim.o.swapfile = false
vim.o.backup = false

-- Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

-- Decrease update time
vim.o.updatetime = 250
vim.wo.signcolumn = 'yes'

vim.o.termguicolors = true
vim.opt.guifont = "JetBrainsMono Nerd Font:h11"

vim.o.completeopt = 'menuone,noselect'

vim.o.scroll = 8
vim.o.scrolloff = 8
vim.o.sidescroll = 4
vim.o.number = true
vim.o.relativenumber = true

vim.o.clipboard = "unnamed,unnamedplus"

vim.o.tabstop = 2
vim.o.shiftwidth = 2
vim.o.smarttab = 2
vim.o.softtabstop = 2
vim.o.expandtab = true
vim.o.smartindent = true

vim.o.wrap = false
