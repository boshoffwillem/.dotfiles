-- [[ Configure Telescope ]]
-- See `:help telescope` and `:help telescope.setup()`
local project_actions = require("telescope._extensions.project.actions")

require('telescope').setup {
  defaults = {
    layout_strategy = "vertical",
    path_display = "absolute",
    mappings = {
      i = {
        ["<C-h>"] = "which_key"
      },
    },
  },
  pickers = {
    buffers = {
      theme = "ivy",
      layout_strategy = "vertical"
    },
    current_buffer_fuzzy_find = {
      theme = "ivy",
      layout_strategy = "vertical"
    },
    diagnostics = {
      theme = "ivy",
      layout_strategy = "vertical"
    },
    find_files = {
      theme = "ivy",
      layout_strategy = "vertical"
    },
    grep_string = {
      theme = "ivy",
      layout_strategy = "vertical"
    },
    help_tags = {
      theme = "ivy",
      layout_strategy = "vertical"
    },
    live_grep = {
      theme = "ivy",
      layout_strategy = "vertical"
    },
    old_files = {
      theme = "ivy",
      layout_strategy = "vertical"
    }
  },
  extensions = {
    project = {
      base_dirs = {
        {path = '~/code', max_depth = 2},
        {path = '~/code/work', max_depth = 2},
        -- {path = '~/code/work/Psicle.Base.Worktrees', max_depth = 2},
      },
      hidden_files = true, -- default: false
      theme = "ivy",
      order_by = "asc",
      search_by = "title",
      sync_with_nvim_tree = false, -- default false
      -- default for on_project_selected = find project files
      on_project_selected = function(prompt_bufnr)
        -- Do anything you want in here. For example:
        project_actions.change_working_directory(prompt_bufnr, false)
        require("harpoon.ui").nav_file(1)
      end
    },
    fzf = {}
  }
}

pcall(require('telescope').load_extension, 'fzf')
pcall(require('telescope').load_extension, 'project')

-- See `:help telescope.builtin`
vim.keymap.set('n', '<leader><space>', require('telescope.builtin').buffers, { desc = '[F]ind [B]uffers' })
vim.keymap.set('n', '<leader>/', require('telescope.builtin').current_buffer_fuzzy_find, { desc = '[/] Find text in current buffer' })
vim.keymap.set('n', '<leader>pf', require('telescope.builtin').find_files, { desc = '[F]ind [F]ile in project' })
vim.keymap.set('n', '<leader>sr', require('telescope.builtin').oldfiles, { desc = '[F]ind [R]ecent File' })
vim.keymap.set('n', '<leader>sh', require('telescope.builtin').help_tags, { desc = '[F]ind [H]elp' })
vim.keymap.set('n', '<leader>sw', require('telescope.builtin').grep_string, { desc = '[F]ind [W]ord in project' })
vim.keymap.set('n', '<leader>ps', require('telescope.builtin').live_grep, { desc = '[F]ind [T]ext in project' })
vim.keymap.set('n', '<leader>pd', require('telescope.builtin').diagnostics, { desc = '[S]earch [D]iagnostics in project' })
vim.keymap.set('n', '<leader>pp',require('telescope').extensions.project.project, { desc = '[F]ind [P]roject' })
