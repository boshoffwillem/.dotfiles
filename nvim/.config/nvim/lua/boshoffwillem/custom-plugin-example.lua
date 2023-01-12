-- define some functionality
function Todo()
  print("Plugin Hello")
end
-- This functionality can be invoked with :lua Todo()

-- This is tedios to type so make it more convenient
vim.api.nvim_create_user_command("Todo", Todo, {})
-- Invoke this with :Todo

-- Maybe you want this command to trigger automatically
vim.api.nvim_create_autocmd("CursorHold", { callback = Todo })

-- Assign a keymap to it
vim.keymap.set("n", "<leader>1", Todo)

-- To provide this funtionality as a plugin
-- simply create your git repo,
-- create a 'lua' folder,
-- then add a lua file, for example, first.lua,
-- and provide the funtionality, for example,
local M = {}

M.todo = function() print("First Plugin") end

return M

-- for faster local development start nvim with a modified rtp:
-- nvim --cmd 'set rtp+=.' lua/first.lua
-- then you can enter :lua require('first').todo() to execute it.
-- You can setup a user command to clear the cache to get updates
vim.api.nvim_create_user_command("Test", function()
  package.loaded.first = nil
  require("first").todo()
end, {})
