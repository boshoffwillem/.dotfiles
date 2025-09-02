local opts = { noremap = true, silent = true }
local term_opts = { silent = true }

-- Shorten function name
local keymap = vim.api.nvim_set_keymap

--Remap space as leader key (already set in init.lua)
keymap("", "<Space>", "<Nop>", opts)

-- Modes
--   normal_mode = "n",
--   insert_mode = "i",
--   visual_mode = "v",
--   visual_block_mode = "x",
--   term_mode = "t",
--   command_mode = "c",

-- Normal --
-- keymap("n", "<leader>e", ":NvimTreeToggle<cr>", opts)

-- Remap h,j,k,l to be on the home row: j, k, l, ;
keymap("n", "j", "h", opts)
keymap("n", "k", "j", opts)
keymap("n", "l", "k", opts)
keymap("n", ";", "l", opts)
keymap("n", "<leader>bk", ":bdelete<CR>", opts)
keymap("n", "<A-n>", ":cnext<CR>", opts)
keymap("n", "<A-p>", ":cprevious<CR>", opts)

-- Yank rest of line
-- =========================================================================================================
keymap("n", "Y", "yg$", opts)
-- =========================================================================================================

-- Move text up and down
-- =========================================================================================================
keymap("n", "<A-k>", ":m .+1<CR>==", opts)
keymap("n", "<A-l>", ":m .-2<CR>==", opts)
-- =========================================================================================================

-- Always center text when scrolling
keymap("n", "<C-d>", "<C-d>zz", opts)
keymap("n", "<C-u>", "<C-u>zz", opts)

-- Folding keymaps
keymap("n", "za", "za", opts) -- toggle fold under cursor
keymap("n", "zA", "zA", opts) -- toggle all folds under cursor recursively
keymap("n", "zo", "zo", opts) -- open fold under cursor
keymap("n", "zO", "zO", opts) -- open all folds under cursor recursively
keymap("n", "zc", "zc", opts) -- close fold under cursor
keymap("n", "zC", "zC", opts) -- close all folds under cursor recursively
keymap("n", "zr", "zr", opts) -- open one level of folds
keymap("n", "zR", "zR", opts) -- open all folds
keymap("n", "zm", "zm", opts) -- close one level of folds
keymap("n", "zM", "zM", opts) -- close all folds
keymap("n", "zv", "zv", opts) -- expand folds to reveal cursor
keymap("n", "<leader>zf", ":foldclose<CR>", opts) -- close all folds
keymap("n", "<leader>zo", ":foldopen<CR>", opts) -- open all folds

-- Insert Mode
-- =========================================================================================================
-- Move text up and down
keymap("i", "<A-k>", ":m .+1<CR>==gi", opts)
keymap("i", "<A-l>", ":m .-2<CR>==gi", opts)
-- =========================================================================================================

-- Visual Mode
-- Remap h,j,k,l to be on the home row: j, k, l, ;
keymap("v", "j", "h", opts)
keymap("v", "k", "j", opts)
keymap("v", "l", "k", opts)
keymap("v", ";", "l", opts)

-- =========================================================================================================
-- Stay in indent mode
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)
-- Move text up and down
keymap("v", "<A-k>", ":m .-2<CR>==", opts)
keymap("v", "<A-j>", ":m .+1<CR>==", opts)
keymap("v", "p", '"_dP', opts)
-- =========================================================================================================

-- Visual Block Mode
-- =========================================================================================================
-- Move text up and down
keymap("x", "J", ":move '>+1<CR>gv-gv", opts)
keymap("x", "K", ":move '<-2<CR>gv-gv", opts)
keymap("x", "<A-j>", ":move '>+1<CR>gv-gv", opts)
keymap("x", "<A-k>", ":move '<-2<CR>gv-gv", opts)
-- =========================================================================================================

-- Terminal
-- =========================================================================================================
-- Better terminal navigation
keymap("t", "<C-h>", "<C-\\><C-N><C-w>h", term_opts)
keymap("t", "<C-j>", "<C-\\><C-N><C-w>j", term_opts)
keymap("t", "<C-k>", "<C-\\><C-N><C-w>k", term_opts)
keymap("t", "<C-l>", "<C-\\><C-N><C-w>l", term_opts)
keymap("t", "<C-q>", "<C-\\><C-N><C-w>q", term_opts)
-- =========================================================================================================
