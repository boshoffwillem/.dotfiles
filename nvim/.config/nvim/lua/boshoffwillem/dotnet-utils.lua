local M = {}

-- Find solution file
local function find_solution_file()
  local cwd = vim.fn.getcwd()
  local sln_files = vim.fn.glob(cwd .. "/*.sln", false, true)
  if #sln_files > 0 then
    return sln_files[1]
  end
  return nil
end

-- Find project files
local function find_project_files()
  local cwd = vim.fn.getcwd()
  local project_files = {}
  
  -- Look for .csproj files
  local csproj_files = vim.fn.glob(cwd .. "/**/*.csproj", false, true)
  vim.list_extend(project_files, csproj_files)
  
  -- Look for .fsproj files
  local fsproj_files = vim.fn.glob(cwd .. "/**/*.fsproj", false, true)
  vim.list_extend(project_files, fsproj_files)
  
  return project_files
end

-- Get project root (solution or closest project)
function M.get_project_root()
  local sln = find_solution_file()
  if sln then
    return vim.fn.fnamemodify(sln, ":h")
  end
  
  local projects = find_project_files()
  if #projects > 0 then
    return vim.fn.fnamemodify(projects[1], ":h")
  end
  
  return vim.fn.getcwd()
end

-- Build solution or project
function M.build()
  local root = M.get_project_root()
  local sln = find_solution_file()
  
  vim.cmd("cd " .. root)
  
  if sln then
    vim.cmd('TermExec cmd="dotnet build"')
  else
    local projects = find_project_files()
    if #projects > 0 then
      vim.cmd('TermExec cmd="dotnet build ' .. projects[1] .. '"')
    else
      vim.notify("No solution or project file found", vim.log.levels.ERROR)
    end
  end
end

-- Clean solution or project
function M.clean()
  local root = M.get_project_root()
  vim.cmd("cd " .. root)
  vim.cmd('TermExec cmd="dotnet clean"')
end

-- Restore packages
function M.restore()
  local root = M.get_project_root()
  vim.cmd("cd " .. root)
  vim.cmd('TermExec cmd="dotnet restore"')
end

-- Run tests
function M.run_tests()
  local root = M.get_project_root()
  vim.cmd("cd " .. root)
  vim.cmd('TermExec cmd="dotnet test"')
end

-- Run current test method
function M.run_current_test()
  local current_line = vim.api.nvim_win_get_cursor(0)[1]
  local bufnr = vim.api.nvim_get_current_buf()
  
  -- Search for test method above current line
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, current_line, false)
  local test_method = nil
  local class_name = nil
  
  -- Find class name
  for i = current_line, 1, -1 do
    local line = lines[i]
    if line then
      local class_match = line:match("public%s+class%s+([%w_]+)")
      if class_match then
        class_name = class_match
        break
      end
    end
  end
  
  -- Find test method
  for i = current_line, 1, -1 do
    local line = lines[i]
    if line then
      local method_match = line:match("public.*void%s+([%w_]+)%(")
      if method_match then
        -- Check if this method has test attributes
        for j = math.max(1, i - 10), i do
          local attr_line = lines[j]
          if attr_line and (attr_line:match("%[Test%]") or attr_line:match("%[Fact%]") or attr_line:match("%[Theory%]")) then
            test_method = method_match
            break
          end
        end
        if test_method then break end
      end
    end
  end
  
  if test_method and class_name then
    local filter = class_name .. "." .. test_method
    local root = M.get_project_root()
    vim.cmd("cd " .. root)
    vim.cmd('TermExec cmd="dotnet test --filter FullyQualifiedName~' .. filter .. '"')
  else
    vim.notify("No test method found at cursor", vim.log.levels.WARN)
  end
end

-- Watch tests
function M.watch_tests()
  local root = M.get_project_root()
  vim.cmd("cd " .. root)
  vim.cmd('TermExec cmd="dotnet watch test"')
end

-- Add package
function M.add_package(package_name)
  if not package_name or package_name == "" then
    package_name = vim.fn.input("Package name: ")
  end
  
  if package_name ~= "" then
    local root = M.get_project_root()
    vim.cmd("cd " .. root)
    vim.cmd('TermExec cmd="dotnet add package ' .. package_name .. '"')
  end
end

-- Remove package
function M.remove_package(package_name)
  if not package_name or package_name == "" then
    package_name = vim.fn.input("Package name to remove: ")
  end
  
  if package_name ~= "" then
    local root = M.get_project_root()
    vim.cmd("cd " .. root)
    vim.cmd('TermExec cmd="dotnet remove package ' .. package_name .. '"')
  end
end

-- Create new project
function M.new_project()
  local project_type = vim.fn.input("Project type (console/classlib/web/api/test): ", "console")
  local project_name = vim.fn.input("Project name: ")
  
  if project_name ~= "" then
    local template_map = {
      console = "console",
      classlib = "classlib",
      web = "mvc",
      api = "webapi",
      test = "mstest"
    }
    
    local template = template_map[project_type] or "console"
    vim.cmd('TermExec cmd="dotnet new ' .. template .. ' -n ' .. project_name .. '"')
  end
end

-- Setup keymaps for .NET development
function M.setup_keymaps()
  local opts = { noremap = true, silent = true }
  
  vim.keymap.set("n", "<leader>db", M.build, vim.tbl_extend("force", opts, { desc = "Build .NET project" }))
  vim.keymap.set("n", "<leader>dc", M.clean, vim.tbl_extend("force", opts, { desc = "Clean .NET project" }))
  vim.keymap.set("n", "<leader>dr", M.restore, vim.tbl_extend("force", opts, { desc = "Restore .NET packages" }))
  -- Universal test keybindings are handled by universal-keybinds.lua
  -- Keep dotnet-specific keybindings with different prefixes
  vim.keymap.set("n", "<leader>dT", M.run_tests, vim.tbl_extend("force", opts, { desc = "Run .NET [T]ests (dotnet specific)" }))
  vim.keymap.set("n", "<leader>dt", M.run_current_test, vim.tbl_extend("force", opts, { desc = "Run current [t]est (dotnet specific)" }))
  vim.keymap.set("n", "<leader>dw", M.watch_tests, vim.tbl_extend("force", opts, { desc = "Watch .NET tests" }))
  vim.keymap.set("n", "<leader>da", M.add_package, vim.tbl_extend("force", opts, { desc = "Add .NET package" }))
  vim.keymap.set("n", "<leader>dR", M.remove_package, vim.tbl_extend("force", opts, { desc = "Remove .NET package" }))
  vim.keymap.set("n", "<leader>dn", M.new_project, vim.tbl_extend("force", opts, { desc = "New .NET project" }))
end

return M
