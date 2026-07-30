return {
  "ibhagwan/fzf-lua",
  -- optional for icon support
  dependencies = { "nvim-tree/nvim-web-devicons" },
  -- or if using mini.icons/mini.nvim
  -- dependencies = { "nvim-mini/mini.icons" },
  ---@module "fzf-lua"
  ---@type fzf-lua.Config|{}
  ---@diagnostic disable: missing-fields
  opts = {},
  keys = {
    {
      "<leader>pf",
      function()
        require("fzf-lua").files()
      end,
      { desc = "Find Files" },
    },
    {
      "<leader>ps",
      function()
        require("fzf-lua").live_grep()
      end,
      { desc = "Find Text" },
    },
  },
}
