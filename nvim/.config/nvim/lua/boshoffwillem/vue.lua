local M = {}

function M.setup()
  -- Vue-specific configuration
  
  -- Set up Vue file associations and patterns
  vim.filetype.add({
    extension = {
      vue = "vue",
    },
    filename = {
      ["vue.config.js"] = "javascript",
      ["vite.config.js"] = "javascript",
      ["vite.config.ts"] = "typescript",
      ["nuxt.config.js"] = "javascript",
      ["nuxt.config.ts"] = "typescript",
    },
    pattern = {
      [".*%.vue$"] = "vue",
    },
  })
  
  -- Vue project detection
  local function is_vue_project()
    return vim.fn.filereadable("vue.config.js") == 1 
        or vim.fn.filereadable("vite.config.js") == 1 
        or vim.fn.filereadable("vite.config.ts") == 1
        or vim.fn.filereadable("nuxt.config.js") == 1
        or vim.fn.filereadable("nuxt.config.ts") == 1
        or vim.fn.isdirectory("node_modules/@vue") == 1
  end
  
  -- Auto-detect Vue project and set up workspace
  if is_vue_project() then
    -- Set up project-specific settings
    vim.opt_local.tabstop = 2
    vim.opt_local.shiftwidth = 2
    vim.opt_local.expandtab = true
  end
  
  -- Vue-specific autocommands
  vim.api.nvim_create_autocmd("FileType", {
    pattern = "vue",
    callback = function()
      -- Vue file specific settings
      vim.opt_local.tabstop = 2
      vim.opt_local.shiftwidth = 2
      vim.opt_local.expandtab = true
      
      -- Set up syntax highlighting for Vue SFCs
      vim.cmd([[
        syntax include @HTML syntax/html.vim
        syntax include @CSS syntax/css.vim
        syntax include @JS syntax/javascript.vim
        syntax include @TS syntax/typescript.vim
      ]])
    end
  })
end

function M.setup_keymaps()
  local opts = { noremap = true, silent = true }
  
  -- Vue-specific keymaps (only set if in Vue project)
  local function is_vue_project()
    return vim.fn.filereadable("vue.config.js") == 1 
        or vim.fn.filereadable("vite.config.js") == 1 
        or vim.fn.filereadable("vite.config.ts") == 1
        or vim.fn.filereadable("nuxt.config.js") == 1
        or vim.fn.filereadable("nuxt.config.ts") == 1
        or vim.fn.isdirectory("node_modules/@vue") == 1
  end
  
  if is_vue_project() then
    -- Vue CLI / Vite commands
    vim.keymap.set("n", "<leader>vb", ":ToggleTerm cmd='npm run build'<CR>", 
      vim.tbl_extend("force", opts, { desc = "Vue build" }))
    vim.keymap.set("n", "<leader>vs", ":ToggleTerm cmd='npm run serve || npm run dev'<CR>", 
      vim.tbl_extend("force", opts, { desc = "Vue serve/dev" }))
    vim.keymap.set("n", "<leader>vt", ":ToggleTerm cmd='npm run test'<CR>", 
      vim.tbl_extend("force", opts, { desc = "Vue test" }))
    vim.keymap.set("n", "<leader>vl", ":ToggleTerm cmd='npm run lint'<CR>", 
      vim.tbl_extend("force", opts, { desc = "Vue lint" }))
    vim.keymap.set("n", "<leader>vp", ":ToggleTerm cmd='npm run preview'<CR>", 
      vim.tbl_extend("force", opts, { desc = "Vue preview" }))
    
    -- Nuxt specific commands (if Nuxt detected)
    if vim.fn.filereadable("nuxt.config.js") == 1 or vim.fn.filereadable("nuxt.config.ts") == 1 then
      vim.keymap.set("n", "<leader>vn", ":ToggleTerm cmd='npm run dev'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Nuxt dev" }))
      vim.keymap.set("n", "<leader>vg", ":ToggleTerm cmd='npm run generate'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Nuxt generate" }))
    end
    
    -- Package management
    vim.keymap.set("n", "<leader>vn", function()
      local package = vim.fn.input("Package name: ")
      if package ~= "" then
        vim.cmd("ToggleTerm cmd='npm install " .. package .. "'")
      end
    end, vim.tbl_extend("force", opts, { desc = "Install npm package" }))
    
    vim.keymap.set("n", "<leader>vD", function()
      local package = vim.fn.input("Package name: ")
      if package ~= "" then
        vim.cmd("ToggleTerm cmd='npm install -D " .. package .. "'")
      end
    end, vim.tbl_extend("force", opts, { desc = "Install dev dependency" }))
    
    -- Vue update
    vim.keymap.set("n", "<leader>vu", ":ToggleTerm cmd='npm update'<CR>", 
      vim.tbl_extend("force", opts, { desc = "Update packages" }))
  end
  
  -- Vue SFC specific keymaps (available in .vue files)
  vim.api.nvim_create_autocmd("FileType", {
    pattern = "vue",
    callback = function()
      -- Navigate between template, script, and style blocks
      vim.keymap.set("n", "<leader>vt", function()
        vim.cmd("/<template")
      end, { buffer = true, desc = "Go to template" })
      
      vim.keymap.set("n", "<leader>vs", function()
        vim.cmd("/<script")
      end, { buffer = true, desc = "Go to script" })
      
      vim.keymap.set("n", "<leader>vc", function()
        vim.cmd("/<style")
      end, { buffer = true, desc = "Go to style" })
      
      -- Vue-specific LSP actions
      vim.keymap.set("n", "<leader>vo", function()
        vim.lsp.buf.code_action({
          filter = function(action)
            return action.title:match("Organize Imports") or action.title:match("Sort Imports")
          end,
          apply = true,
        })
      end, { buffer = true, desc = "Organize imports" })
    end
  })
  
  -- JavaScript/TypeScript keymaps for Vue files
  vim.api.nvim_create_autocmd("FileType", {
    pattern = { "vue", "javascript", "typescript" },
    callback = function()
      -- Common JS/TS actions in Vue context
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