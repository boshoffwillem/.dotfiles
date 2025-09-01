-- Debugger installation location
local plugins_path = vim.fn.stdpath("data")
local install_dir = plugins_path .. "/mason"

vim.keymap.set("n", "<leader>lb", function()
  require("dap").toggle_breakpoint()
end, { desc = "[T]oggle [B]reakpoint" })
vim.keymap.set("n", "<leader>lB", function()
  require("dap").set_breakpoint(vim.fn.input("Breakpoint condition: "))
end, { desc = "[C]onditional [B]reakpoint" })
vim.keymap.set("n", "<leader>ll", function()
  require("dap").set_breakpoint(nil, nil, vim.fn.input("Log point message: "))
end, { desc = "[L]og [P]oint" })
vim.keymap.set("n", "<leader>x", function()
  require("dap").repl.open()
end, { desc = "[R]epl" })
vim.keymap.set("n", "<F5>", function()
  require("dap").continue()
end, { desc = "[C]ontinue" })
vim.keymap.set("n", "<F10>", function()
  require("dap").step_over()
end, { desc = "[S]tep [O]ver" })
vim.keymap.set("n", "<F11>", function()
  require("dap").step_into()
end, { desc = "[S]tep [I]nto" })
vim.keymap.set("n", "<F12>", function()
  require("dap").step_out()
end, { desc = "[S]tep [O]ut" })

-- .NET setup
local dap = require("dap")

dap.adapters.coreclr = {
  type = "executable",
  command = install_dir .. "/packages/netcoredbg/netcoredbg",
  args = { "--interpreter=vscode" },
}

-- Function to find .NET executable
local function get_dotnet_executable()
  local cwd = vim.fn.getcwd()
  local possible_paths = {
    cwd .. "/bin/Debug/net8.0/",
    cwd .. "/bin/Debug/net7.0/",
    cwd .. "/bin/Debug/net6.0/",
    cwd .. "/bin/Debug/netcoreapp3.1/",
  }
  
  for _, path in ipairs(possible_paths) do
    local files = vim.fn.glob(path .. "*.dll", false, true)
    if #files > 0 then
      -- Return the first .dll found that matches the project name
      local project_name = vim.fn.fnamemodify(cwd, ":t")
      for _, file in ipairs(files) do
        if file:match(project_name) then
          return file
        end
      end
      -- If no match found, return the first dll
      return files[1]
    end
  end
  
  return vim.fn.input("Path to dll: ", vim.fn.getcwd() .. "/bin/Debug/", "file")
end

dap.configurations.cs = {
  {
    type = "coreclr",
    name = "Launch .NET Core",
    request = "launch",
    program = get_dotnet_executable,
    args = {},
    cwd = "${workspaceFolder}",
    console = "integratedTerminal",
    stopAtEntry = false,
  },
  {
    type = "coreclr",
    name = "Launch with args",
    request = "launch",
    program = get_dotnet_executable,
    args = function()
      local args = vim.fn.input("Program arguments: ")
      return vim.split(args, " ")
    end,
    cwd = "${workspaceFolder}",
    console = "integratedTerminal",
    stopAtEntry = false,
  },
  {
    type = "coreclr",
    name = "Attach to process",
    request = "attach",
    processId = function()
      return require("dap.utils").pick_process()
    end,
  },
}

require("dapui").setup({
  icons = { expanded = "", collapsed = "", current_frame = "" },
  mappings = {
    -- Use a table to apply multiple mappings
    expand = { "<CR>", "<2-LeftMouse>" },
    open = "o",
    remove = "d",
    edit = "e",
    repl = "r",
    toggle = "t",
  },
  -- Use this to override mappings for specific elements
  element_mappings = {
    -- Example:
    -- stacks = {
    --   open = "<CR>",
    --   expand = "o",
    -- }
  },
  -- Expand lines larger than the window
  -- Requires >= 0.7
  expand_lines = vim.fn.has("nvim-0.7") == 1,
  -- Layouts define sections of the screen to place windows.
  -- The position can be "left", "right", "top" or "bottom".
  -- The size specifies the height/width depending on position. It can be an Int
  -- or a Float. Integer specifies height/width directly (i.e. 20 lines/columns) while
  -- Float value specifies percentage (i.e. 0.3 - 30% of available lines/columns)
  -- Elements are the elements shown in the layout (in order).
  -- Layouts are opened in order so that earlier layouts take priority in window sizing.
  layouts = {
    {
      elements = {
        -- Elements can be strings or table with id and size keys.
        { id = "scopes", size = 0.25 },
        "breakpoints",
        "stacks",
        "watches",
      },
      size = 40, -- 40 columns
      position = "left",
    },
    {
      elements = {
        "repl",
        "console",
      },
      size = 0.25, -- 25% of total lines
      position = "bottom",
    },
  },
  controls = {
    -- Requires Neovim nightly (or 0.8 when released)
    enabled = true,
    -- Display controls in this element
    element = "repl",
    icons = {
      pause = "",
      play = "",
      step_into = "",
      step_over = "",
      step_out = "",
      step_back = "",
      run_last = "",
      terminate = "",
    },
  },
  floating = {
    max_height = nil, -- These can be integers or a float between 0 and 1.
    max_width = nil, -- Floats will be treated as percentage of your screen.
    border = "single", -- Border style. Can be "single", "double" or "rounded"
    mappings = {
      close = { "q", "<Esc>" },
    },
  },
  windows = { indent = 1 },
  render = {
    max_type_length = nil, -- Can be integer or nil.
    max_value_lines = 100, -- Can be integer or nil.
  },
})

require("nvim-dap-virtual-text").setup({
  enabled = true, -- enable this plugin (the default)
  enabled_commands = true, -- create commands DapVirtualTextEnable, DapVirtualTextDisable, DapVirtualTextToggle, (DapVirtualTextForceRefresh for refreshing when debug adapter did not notify its termination)
  highlight_changed_variables = true, -- highlight changed values with NvimDapVirtualTextChanged, else always NvimDapVirtualText
  highlight_new_as_changed = false, -- highlight new variables in the same way as changed variables (if highlight_changed_variables)
  show_stop_reason = true, -- show stop reason when stopped for exceptions
  commented = false, -- prefix virtual text with comment string
  only_first_definition = true, -- only show virtual text at first definition (if there are multiple)
  all_references = false, -- show virtual text on all all references of the variable (not only definitions)
  clear_on_continue = false, -- clear virtual text on "continue" (might cause flickering when stepping)
  --- A callback that determines how a variable is displayed or whether it should be omitted
  --- @param variable Variable https://microsoft.github.io/debug-adapter-protocol/specification#Types_Variable
  --- @param buf number
  --- @param stackframe dap.StackFrame https://microsoft.github.io/debug-adapter-protocol/specification#Types_StackFrame
  --- @param node userdata tree-sitter node identified as variable definition of reference (see `:h tsnode`)
  --- @param options nvim_dap_virtual_text_options Current options for nvim-dap-virtual-text
  --- @return string|nil A text how the virtual text should be displayed or nil, if this variable shouldn't be displayed
  display_callback = function(variable, buf, stackframe, node, options)
    -- by default, strip out new line characters
    if options.virt_text_pos == "inline" then
      return " = " .. variable.value:gsub("%s+", " ")
    else
      return variable.name .. " = " .. variable.value:gsub("%s+", " ")
    end
  end,
  -- position of virtual text, see `:h nvim_buf_set_extmark()`, default tries to inline the virtual text. Use 'eol' to set to end of line
  virt_text_pos = vim.fn.has("nvim-0.10") == 1 and "inline" or "eol",

  -- experimental features:
  all_frames = false, -- show virtual text for all stack frames not only current. Only works for debugpy on my machine.
  virt_lines = false, -- show virtual lines instead of virtual text (will flicker!)
  virt_text_win_col = nil, -- position the virtual text at a fixed window column (starting from the first text column) ,
  -- e.g. 80 to position at column 80, see `:h nvim_buf_set_extmark()`
})
