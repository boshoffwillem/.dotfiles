local M = {}

function M.setup()
  local status_ok, neotree = pcall(require, "neo-tree")
  if not status_ok then
    return
  end

  neotree.setup({
    close_if_last_window = true,
    popup_border_style = "rounded",
    enable_git_status = true,
    enable_diagnostics = true,
    sort_case_insensitive = false,
    default_component_configs = {
      container = {
        enable_character_fade = true
      },
      indent = {
        indent_size = 2,
        padding = 1,
        with_markers = true,
        indent_marker = "│",
        last_indent_marker = "└",
        highlight = "NeoTreeIndentMarker",
        with_expanders = nil,
        expander_collapsed = "",
        expander_expanded = "",
        expander_highlight = "NeoTreeExpander",
      },
      icon = {
        folder_closed = "",
        folder_open = "",
        folder_empty = "󰜌",
        default = "*",
        highlight = "NeoTreeFileIcon"
      },
      modified = {
        symbol = "[+]",
        highlight = "NeoTreeModified",
      },
      name = {
        trailing_slash = false,
        use_git_status_colors = true,
        highlight = "NeoTreeFileName",
      },
      git_status = {
        symbols = {
          added     = "✚",
          modified  = "",
          deleted   = "✖",
          renamed   = "󰁕",
          untracked = "",
          ignored   = "",
          unstaged  = "󰄱",
          staged    = "",
          conflict  = "",
        }
      },
    },
    window = {
      position = "left",
      width = 30,
      mapping_options = {
        noremap = true,
        nowait = true,
      },
      mappings = {
        -- Navigation with j,k,l,; instead of h,j,k,l
        -- j = left (was h)
        -- k = down (was j)  
        -- l = up (was k)
        -- ; = right (was l)
        
        ["h"] = "none",  -- Disable default h
        ["j"] = "close_node",  -- j for left/close (was h)
        ["k"] = function(state)  -- k for down (was j)
          vim.cmd("normal! j")
        end,
        ["l"] = function(state)  -- l for up (was k)
          vim.cmd("normal! k")
        end,
        [";"] = "open",  -- ; for right/open (was l)
        
        ["<space>"] = { 
          "toggle_node", 
          nowait = false,
        },
        ["<2-LeftMouse>"] = "open",
        ["<cr>"] = "open",
        ["<esc>"] = "cancel",
        ["P"] = { "toggle_preview", config = { use_float = true } },
        ["L"] = "focus_preview",  -- Capital L for preview
        ["S"] = "open_split",
        ["s"] = "open_vsplit",
        ["t"] = "open_tabnew",
        ["w"] = "open_with_window_picker",
        ["C"] = "close_node",
        ["z"] = "close_all_nodes",
        ["a"] = { 
          "add",
          config = {
            show_path = "none"
          }
        },
        ["A"] = "add_directory",
        ["d"] = "delete",
        ["r"] = "rename",
        ["y"] = "copy_to_clipboard",
        ["x"] = "cut_to_clipboard",
        ["p"] = "paste_from_clipboard",
        ["c"] = "copy",
        ["m"] = "move",
        ["q"] = "close_window",
        ["R"] = "refresh",
        ["?"] = "show_help",
        ["<"] = "prev_source",
        [">"] = "next_source",
      }
    },
    nesting_rules = {},
    filesystem = {
      filtered_items = {
        visible = false,
        hide_dotfiles = false,
        hide_gitignored = false,
        hide_hidden = false,
        hide_by_name = {
          "node_modules"
        },
        hide_by_pattern = {
          "*.meta",
          "*/src/*/tsconfig.json",
        },
        always_show = {
          ".gitignored",
          ".env",
        },
        never_show = {
          ".DS_Store",
          "thumbs.db"
        },
        never_show_by_pattern = {},
      },
      follow_current_file = {
        enabled = true,
        leave_dirs_open = false,
      },
      group_empty_dirs = false,
      hijack_netrw_behavior = "open_default",
      use_libuv_file_watcher = false,
      window = {
        mappings = {
          ["<bs>"] = "navigate_up",
          ["."] = "set_root",
          ["H"] = "toggle_hidden",
          ["/"] = "fuzzy_finder",
          ["D"] = "fuzzy_finder_directory",
          ["#"] = "fuzzy_sorter",
          ["f"] = "filter_on_submit",
          ["<c-x>"] = "clear_filter",
          ["[g"] = "prev_git_modified",
          ["]g"] = "next_git_modified",
        },
        fuzzy_finder_mappings = {
          ["<down>"] = "move_cursor_down",
          ["<C-n>"] = "move_cursor_down",
          ["<up>"] = "move_cursor_up",
          ["<C-p>"] = "move_cursor_up",
        },
      },
    },
    buffers = {
      follow_current_file = {
        enabled = true,
        leave_dirs_open = false,
      },
      group_empty_dirs = true,
      show_unloaded = true,
      window = {
        mappings = {
          ["bd"] = "buffer_delete",
          ["<bs>"] = "navigate_up",
          ["."] = "set_root",
        }
      },
    },
    git_status = {
      window = {
        position = "float",
        mappings = {
          ["A"]  = "git_add_all",
          ["gu"] = "git_unstage_file",
          ["ga"] = "git_add_file",
          ["gr"] = "git_revert_file",
          ["gc"] = "git_commit",
          ["gp"] = "git_push",
          ["gg"] = "git_commit_and_push",
        }
      }
    }
  })
end

function M.setup_keymaps()
  local opts = { noremap = true, silent = true }
  
  vim.keymap.set("n", "<leader>o", ":Neotree toggle<CR>", vim.tbl_extend("force", opts, { desc = "Toggle file explorer" }))
  vim.keymap.set("n", "<leader>O", ":Neotree reveal<CR>", vim.tbl_extend("force", opts, { desc = "Reveal current file in explorer" }))
  vim.keymap.set("n", "<leader>ge", ":Neotree git_status<CR>", vim.tbl_extend("force", opts, { desc = "Git status in explorer" }))
  vim.keymap.set("n", "<leader>be", ":Neotree buffers<CR>", vim.tbl_extend("force", opts, { desc = "Buffer explorer" }))
end

return M