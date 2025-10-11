return {
  -- Core Tabnine plugin (binary manager + commands like :TabnineHub)
  {
    "codota/tabnine-nvim",
    build = "./dl_binaries.sh", -- downloads / updates the Tabnine binary
    event = "InsertEnter",
    config = function()
      require("tabnine").setup({
        disable_auto_comment = true,
        accept_keymap = "<C-Y>", -- You can remap; <Tab> often conflicts with snippet jump
        dismiss_keymap = "<C-]>",
        debounce_ms = 300,
        suggestion_color = { gui = "#808080", cterm = 244 },
        exclude_filetypes = { "TelescopePrompt", "spectre_panel" },
        log_file_path = nil, -- set a path to debug (e.g. vim.fn.stdpath("cache") .. "/tabnine.log")
      })

      -- Optional: Keymaps for quick access
      vim.keymap.set("n", "<Leader>th", "<Cmd>TabnineHub<CR>", { desc = "Tabnine: Open Hub" })
      vim.keymap.set("n", "<Leader>ts", "<Cmd>TabnineStatus<CR>", { desc = "Tabnine: Status" })
    end,
  },

  -- nvim-cmp source + comparator for Tabnine
  {
    "tzachar/cmp-tabnine",
    build = "./install.sh",
    dependencies = { "hrsh7th/nvim-cmp" },
    event = "InsertEnter",
    config = function()
      require("cmp_tabnine.config"):setup({
        max_lines = 1000,
        max_num_results = 20,
        sort = true,
        run_on_every_keystroke = true,
        snippet_placeholder = "..",
        ignored_file_types = {
          markdown = true, -- example: disable in markdown
        },
        show_prediction_strength = true,
      })
    end,
  },

  -- Extend existing cmp config to inject Tabnine source & sorting
  {
    "hrsh7th/nvim-cmp",
    optional = true,
    dependencies = { "tzachar/cmp-tabnine" },
    opts = function(_, opts)
      local cmp = require("cmp")

      opts.sources = opts.sources or {}

      -- Insert Tabnine source with a priority. You can move it earlier/later.
      -- Keep existing sources (Kickstart sets them); we just append or adjust.
      local has_tabnine = false
      for _, s in ipairs(opts.sources or {}) do
        if s.name == "cmp_tabnine" then
          has_tabnine = true
          break
        end
      end
      if not has_tabnine then
        table.insert(opts.sources, 2, { name = "cmp_tabnine", priority = 90 })
      end

      -- Prepend Tabnine comparator so its confidence can influence ordering
      local compare = cmp.config.compare
      opts.sorting = opts.sorting or {}
      opts.sorting.priority_weight = 2
      opts.sorting.comparators = vim.list_extend({
        require("cmp_tabnine.compare"),
      }, opts.sorting.comparators or {
        compare.offset,
        compare.exact,
        compare.score,
        compare.recently_used,
        compare.kind,
        compare.sort_text,
        compare.length,
        compare.order,
      })

      -- (Optional) Fancy formatting to show Tabnine strength %
      local lspkind_ok, lspkind = pcall(require, "lspkind")
      opts.formatting = opts.formatting or {}
      opts.formatting.format = function(entry, item)
        if lspkind_ok then
          item = lspkind.cmp_format({ mode = "symbol_text", maxwidth = 50 })(entry, item)
        end
        if entry.source.name == "cmp_tabnine" then
          local detail = (entry.completion_item.labelDetails or {}).detail
          if detail and detail ~= "" then
            item.menu = (item.menu or "") .. " TN(" .. detail .. ")"
          else
            item.menu = (item.menu or "") .. " TabNine"
          end
        end
        return item
      end

      return opts
    end,
  },
}
