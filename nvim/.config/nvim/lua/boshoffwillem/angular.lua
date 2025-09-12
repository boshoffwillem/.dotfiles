local M = {}

function M.setup()
  -- Angular-specific configuration
  
  -- Set up Angular file associations
  vim.filetype.add({
    extension = {
      component = "typescript",
      service = "typescript",
      directive = "typescript",
      guard = "typescript",
      pipe = "typescript",
      resolver = "typescript",
    },
    filename = {
      ["angular.json"] = "json",
      [".angular-cli.json"] = "json",
    },
    pattern = {
      [".*%.component%.ts$"] = "typescript",
      [".*%.service%.ts$"] = "typescript",
      [".*%.directive%.ts$"] = "typescript",
      [".*%.guard%.ts$"] = "typescript",
      [".*%.pipe%.ts$"] = "typescript",
      [".*%.resolver%.ts$"] = "typescript",
      [".*%.module%.ts$"] = "typescript",
    },
  })
  
  -- Angular CLI integration
  local function run_ng_command(args)
    local cmd = "ng " .. args
    vim.cmd("ToggleTerm cmd='" .. cmd .. "'")
  end
  
  -- Angular project detection
  local function is_angular_project()
    return vim.fn.filereadable("angular.json") == 1 or vim.fn.filereadable(".angular-cli.json") == 1
  end
  
  -- Auto-detect Angular project and set up workspace
  if is_angular_project() then
    -- Set up project-specific settings
    vim.opt_local.tabstop = 2
    vim.opt_local.shiftwidth = 2
    vim.opt_local.expandtab = true
  end
end

function M.setup_keymaps()
  local opts = { noremap = true, silent = true }
  
  -- Angular-specific keymaps (only set if in Angular project)
  local function is_angular_project()
    return vim.fn.filereadable("angular.json") == 1 or vim.fn.filereadable(".angular-cli.json") == 1
  end
  
  if is_angular_project() then
    -- Angular CLI commands
    vim.keymap.set("n", "<leader>ab", ":ToggleTerm cmd='ng build'<CR>", 
      vim.tbl_extend("force", opts, { desc = "Angular build" }))
    vim.keymap.set("n", "<leader>as", ":ToggleTerm cmd='ng serve'<CR>", 
      vim.tbl_extend("force", opts, { desc = "Angular serve" }))
    vim.keymap.set("n", "<leader>at", ":ToggleTerm cmd='ng test'<CR>", 
      vim.tbl_extend("force", opts, { desc = "Angular test" }))
    vim.keymap.set("n", "<leader>ae", ":ToggleTerm cmd='ng e2e'<CR>", 
      vim.tbl_extend("force", opts, { desc = "Angular e2e tests" }))
    vim.keymap.set("n", "<leader>al", ":ToggleTerm cmd='ng lint'<CR>", 
      vim.tbl_extend("force", opts, { desc = "Angular lint" }))
    
    -- Generate commands
    vim.keymap.set("n", "<leader>agc", function()
      local name = vim.fn.input("Component name: ")
      if name ~= "" then
        vim.cmd("ToggleTerm cmd='ng generate component " .. name .. "'")
      end
    end, vim.tbl_extend("force", opts, { desc = "Generate component" }))
    
    vim.keymap.set("n", "<leader>ags", function()
      local name = vim.fn.input("Service name: ")
      if name ~= "" then
        vim.cmd("ToggleTerm cmd='ng generate service " .. name .. "'")
      end
    end, vim.tbl_extend("force", opts, { desc = "Generate service" }))
    
    vim.keymap.set("n", "<leader>agd", function()
      local name = vim.fn.input("Directive name: ")
      if name ~= "" then
        vim.cmd("ToggleTerm cmd='ng generate directive " .. name .. "'")
      end
    end, vim.tbl_extend("force", opts, { desc = "Generate directive" }))
    
    vim.keymap.set("n", "<leader>agp", function()
      local name = vim.fn.input("Pipe name: ")
      if name ~= "" then
        vim.cmd("ToggleTerm cmd='ng generate pipe " .. name .. "'")
      end
    end, vim.tbl_extend("force", opts, { desc = "Generate pipe" }))
    
    vim.keymap.set("n", "<leader>agm", function()
      local name = vim.fn.input("Module name: ")
      if name ~= "" then
        vim.cmd("ToggleTerm cmd='ng generate module " .. name .. "'")
      end
    end, vim.tbl_extend("force", opts, { desc = "Generate module" }))
    
    -- Package management
    vim.keymap.set("n", "<leader>an", function()
      local package = vim.fn.input("Package name: ")
      if package ~= "" then
        vim.cmd("ToggleTerm cmd='npm install " .. package .. "'")
      end
    end, vim.tbl_extend("force", opts, { desc = "Install npm package" }))
    
    vim.keymap.set("n", "<leader>aD", function()
      local package = vim.fn.input("Package name: ")
      if package ~= "" then
        vim.cmd("ToggleTerm cmd='npm install -D " .. package .. "'")
      end
    end, vim.tbl_extend("force", opts, { desc = "Install dev dependency" }))
    
    -- Angular update
    vim.keymap.set("n", "<leader>au", ":ToggleTerm cmd='ng update'<CR>", 
      vim.tbl_extend("force", opts, { desc = "Angular update" }))
  end
  
  -- TypeScript-specific keymaps (available in all TypeScript files)
  vim.api.nvim_create_autocmd("FileType", {
    pattern = { "typescript", "typescriptreact" },
    callback = function()
      -- TypeScript specific actions
      vim.keymap.set("n", "<leader>to", function()
        vim.lsp.buf.code_action({
          filter = function(action)
            return action.title:match("Organize Imports") or action.title:match("Sort Imports")
          end,
          apply = true,
        })
      end, { buffer = true, desc = "Organize imports" })
      
      vim.keymap.set("n", "<leader>tI", function()
        vim.lsp.buf.code_action({
          filter = function(action)
            return action.title:match("Add all missing imports")
          end,
          apply = true,
        })
      end, { buffer = true, desc = "Add missing imports" })
      
      vim.keymap.set("n", "<leader>tr", function()
        vim.lsp.buf.code_action({
          filter = function(action)
            return action.title:match("Remove unused")
          end,
          apply = true,
        })
      end, { buffer = true, desc = "Remove unused imports" })
    end
  })
end

return M