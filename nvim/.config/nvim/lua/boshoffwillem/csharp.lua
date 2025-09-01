local M = {}

-- Setup C# specific configurations
function M.setup()
  -- Configure roslyn.nvim if available
  local has_roslyn, roslyn = pcall(require, "roslyn")
  if has_roslyn then
    roslyn.setup({
      config = {
        on_attach = function(client, bufnr)
          client.server_capabilities.semanticTokensProvider = nil
          require("boshoffwillem.lsp").on_attach(client, bufnr)
        end,
        capabilities = require("cmp_nvim_lsp").default_capabilities(),
      },
      exe = {
        "dotnet",
        vim.fs.joinpath(vim.fn.stdpath("data"), "roslyn", "Microsoft.CodeAnalysis.LanguageServer.dll"),
      },
      filetypes = { "cs" },
      settings = {
        ["csharp|inlay_hints"] = {
          ["csharp|inlay_hints|enable_inlay_hints_for_parameters"] = true,
          ["csharp|inlay_hints|enable_inlay_hints_for_literal_parameters"] = true,
          ["csharp|inlay_hints|enable_inlay_hints_for_indexer_parameters"] = true,
          ["csharp|inlay_hints|enable_inlay_hints_for_object_creation_parameters"] = true,
          ["csharp|inlay_hints|enable_inlay_hints_for_other_parameters"] = true,
          ["csharp|inlay_hints|suppress_inlay_hints_for_parameters_that_differ_only_by_suffix"] = false,
          ["csharp|inlay_hints|suppress_inlay_hints_for_parameters_that_match_method_intent"] = false,
          ["csharp|inlay_hints|suppress_inlay_hints_for_parameters_that_match_argument_name"] = false,
          ["csharp|inlay_hints|enable_inlay_hints_for_types"] = true,
          ["csharp|inlay_hints|enable_inlay_hints_for_implicit_variable_types"] = true,
          ["csharp|inlay_hints|enable_inlay_hints_for_lambda_parameter_types"] = true,
          ["csharp|inlay_hints|enable_inlay_hints_for_implicit_object_creation"] = true,
        },
        ["csharp|code_lens"] = {
          ["dotnet_enable_references_code_lens"] = true,
        },
        ["csharp|completion"] = {
          ["dotnet_provide_regex_completions"] = true,
          ["dotnet_show_completion_items_from_unimported_namespaces"] = true,
        },
      }
    })
  end
  
  -- Configure csharp.nvim if available
  local has_csharp, csharp = pcall(require, "csharp")
  if has_csharp then
    csharp.setup({
      lsp = {
        omnisharp = {
          cmd = { "omnisharp" },
          enable_roslyn_analyzers = true,
          enable_import_completion = true,
          organize_imports_on_format = true,
          enable_decompilation_support = true,
        }
      },
      logging = {
        level = "INFO",
      },
      dap = {
        adapter_name = "coreclr",
        -- Path to debugger (will be set in dap.lua)
      }
    })
  end
  
  -- Configure lsp-overloads if available
  local has_overloads, overloads = pcall(require, "lsp-overloads")
  if has_overloads then
    overloads.setup({
      keymaps = {
        next_signature = "<C-j>",
        previous_signature = "<C-k>",
        next_parameter = "<C-l>",
        previous_parameter = "<C-h>",
        close_signature = "<A-s>"
      },
      display_automatically = true
    })
  end
end

-- C# specific utility functions
function M.run_single_test()
  local current_line = vim.api.nvim_win_get_cursor(0)[1]
  local bufnr = vim.api.nvim_get_current_buf()
  
  -- Search for test method above current line
  local lines = vim.api.nvim_buf_get_lines(bufnr, 0, current_line, false)
  local test_method = nil
  
  for i = current_line, 1, -1 do
    local line = lines[i]
    if line then
      local method_match = line:match("public.*void%s+([%w_]+)%(")
      if method_match then
        -- Check if this method has [Test] or [Fact] attribute
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
  
  if test_method then
    local cmd = string.format("dotnet test --filter FullyQualifiedName~%s", test_method)
    vim.cmd("TermExec cmd='" .. cmd .. "'")
  else
    vim.notify("No test method found at cursor", vim.log.levels.WARN)
  end
end

function M.run_all_tests()
  vim.cmd("TermExec cmd='dotnet test'")
end

function M.build_solution()
  vim.cmd("TermExec cmd='dotnet build'")
end

function M.restore_packages()
  vim.cmd("TermExec cmd='dotnet restore'")
end

function M.clean_solution()
  vim.cmd("TermExec cmd='dotnet clean'")
end

function M.add_package()
  local package_name = vim.fn.input("Package name: ")
  if package_name ~= "" then
    local cmd = string.format("dotnet add package %s", package_name)
    vim.cmd("TermExec cmd='" .. cmd .. "'")
  end
end

function M.remove_package()
  local package_name = vim.fn.input("Package name to remove: ")
  if package_name ~= "" then
    local cmd = string.format("dotnet remove package %s", package_name)
    vim.cmd("TermExec cmd='" .. cmd .. "'")
  end
end

function M.new_class()
  local class_name = vim.fn.input("Class name: ")
  if class_name ~= "" then
    local template = string.format([[
namespace %s;

public class %s
{
    
}
]], vim.fn.fnamemodify(vim.fn.getcwd(), ":t"), class_name)
    
    local filename = string.format("%s.cs", class_name)
    local full_path = vim.fn.getcwd() .. "/" .. filename
    
    vim.fn.writefile(vim.split(template, "\n"), full_path)
    vim.cmd("edit " .. full_path)
  end
end

function M.new_interface()
  local interface_name = vim.fn.input("Interface name (without I prefix): ")
  if interface_name ~= "" then
    local full_interface_name = "I" .. interface_name
    local template = string.format([[
namespace %s;

public interface %s
{
    
}
]], vim.fn.fnamemodify(vim.fn.getcwd(), ":t"), full_interface_name)
    
    local filename = string.format("%s.cs", full_interface_name)
    local full_path = vim.fn.getcwd() .. "/" .. filename
    
    vim.fn.writefile(vim.split(template, "\n"), full_path)
    vim.cmd("edit " .. full_path)
  end
end

function M.setup_keymaps()
  local opts = { noremap = true, silent = true }
  
  -- Test running
  vim.keymap.set("n", "<leader>tt", M.run_single_test, vim.tbl_extend("force", opts, { desc = "Run single test" }))
  vim.keymap.set("n", "<leader>ta", M.run_all_tests, vim.tbl_extend("force", opts, { desc = "Run all tests" }))
  
  -- Build commands
  vim.keymap.set("n", "<leader>cb", M.build_solution, vim.tbl_extend("force", opts, { desc = "Build solution" }))
  vim.keymap.set("n", "<leader>cr", M.restore_packages, vim.tbl_extend("force", opts, { desc = "Restore packages" }))
  vim.keymap.set("n", "<leader>cx", M.clean_solution, vim.tbl_extend("force", opts, { desc = "Clean solution" }))
  
  -- Package management
  vim.keymap.set("n", "<leader>pa", M.add_package, vim.tbl_extend("force", opts, { desc = "Add package" }))
  vim.keymap.set("n", "<leader>pr", M.remove_package, vim.tbl_extend("force", opts, { desc = "Remove package" }))
  
  -- File creation
  vim.keymap.set("n", "<leader>nc", M.new_class, vim.tbl_extend("force", opts, { desc = "New class" }))
  vim.keymap.set("n", "<leader>ni", M.new_interface, vim.tbl_extend("force", opts, { desc = "New interface" }))
end

-- Setup autocommands for C# files
function M.setup_autocommands()
  local augroup = vim.api.nvim_create_augroup("CSharp", { clear = true })
  
  -- Set specific settings for C# files
  vim.api.nvim_create_autocmd("FileType", {
    pattern = "cs",
    group = augroup,
    callback = function()
      vim.opt_local.tabstop = 4
      vim.opt_local.shiftwidth = 4
      vim.opt_local.expandtab = true
      vim.opt_local.textwidth = 120
      vim.opt_local.colorcolumn = "120"
      
      -- Enable inlay hints if supported
      if vim.lsp.inlay_hint then
        vim.lsp.inlay_hint.enable(true, { bufnr = 0 })
      end
    end,
  })
  
  -- Auto-format on save
  vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = "*.cs",
    group = augroup,
    callback = function()
      vim.lsp.buf.format({ async = false })
    end,
  })
end

return M