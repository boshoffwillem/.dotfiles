local M = {}

function M.setup()
  -- Go-specific configuration
  
  -- Set up Go file settings
  vim.api.nvim_create_autocmd("FileType", {
    pattern = "go",
    callback = function()
      -- Go prefers tabs over spaces
      vim.opt_local.expandtab = false
      vim.opt_local.tabstop = 4
      vim.opt_local.shiftwidth = 4
      vim.opt_local.softtabstop = 4
      
      -- Auto-format and organize imports on save
      local group = vim.api.nvim_create_augroup("GoFormat", { clear = false })
      vim.api.nvim_create_autocmd("BufWritePre", {
        pattern = "*.go",
        group = group,
        callback = function()
          -- Organize imports
          local params = vim.lsp.util.make_range_params()
          params.context = { only = { "source.organizeImports" } }
          local result = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params, 3000)
          for _, res in pairs(result or {}) do
            for _, r in pairs(res.result or {}) do
              if r.edit then
                vim.lsp.util.apply_workspace_edit(r.edit, "utf-8")
              end
            end
          end
          
          -- Format code
          vim.lsp.buf.format({ async = false })
        end,
      })
    end
  })
  
  -- Go module detection
  local function is_go_project()
    return vim.fn.filereadable("go.mod") == 1
  end
  
  -- Set up Go-specific features if go.nvim is available
  local has_go_nvim, go_nvim = pcall(require, "go")
  if has_go_nvim then
    go_nvim.setup({
      -- go.nvim configuration
      go = "go",
      goimports = "gopls",
      gofmt = "gopls",
      tag_transform = false,
      test_dir = "",
      comment_placeholder = "   ",
      lsp_cfg = false, -- don't override lsp config
      lsp_gofumpt = true,
      lsp_on_attach = false, -- use universal on_attach
      dap_debug = true,
      dap_debug_keymap = false, -- use universal debug keymaps
      dap_debug_gui = true,
      dap_debug_vt = true,
      build_tags = "",
      textobjects = true,
      test_runner = "go",
      verbose_tests = true,
      run_in_floaterm = false,
      trouble = false,
      test_efm = false,
      luasnip = true,
    })
  end
end

function M.setup_keymaps()
  local opts = { noremap = true, silent = true }
  
  -- Go-specific keymaps (available in all Go files)
  vim.api.nvim_create_autocmd("FileType", {
    pattern = "go",
    callback = function()
      -- Go build and run
      vim.keymap.set("n", "<leader>gb", ":ToggleTerm cmd='go build'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Go build", buffer = true }))
      vim.keymap.set("n", "<leader>gr", ":ToggleTerm cmd='go run .'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Go run", buffer = true }))
      vim.keymap.set("n", "<leader>gR", function()
        local file = vim.fn.expand("%")
        vim.cmd("ToggleTerm cmd='go run " .. file .. "'")
      end, vim.tbl_extend("force", opts, { desc = "Go run current file", buffer = true }))
      
      -- Go testing
      vim.keymap.set("n", "<leader>gt", ":ToggleTerm cmd='go test ./...'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Go test all", buffer = true }))
      vim.keymap.set("n", "<leader>gT", ":ToggleTerm cmd='go test -v ./...'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Go test verbose", buffer = true }))
      vim.keymap.set("n", "<leader>gf", function()
        local dir = vim.fn.expand("%:h")
        vim.cmd("ToggleTerm cmd='go test " .. dir .. "'")
      end, vim.tbl_extend("force", opts, { desc = "Go test current package", buffer = true }))
      
      -- Go module management
      vim.keymap.set("n", "<leader>gm", ":ToggleTerm cmd='go mod tidy'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Go mod tidy", buffer = true }))
      vim.keymap.set("n", "<leader>gd", ":ToggleTerm cmd='go mod download'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Go mod download", buffer = true }))
      vim.keymap.set("n", "<leader>gu", ":ToggleTerm cmd='go get -u ./...'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Go get update", buffer = true }))
      
      -- Go tools
      vim.keymap.set("n", "<leader>gv", ":ToggleTerm cmd='go vet ./...'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Go vet", buffer = true }))
      vim.keymap.set("n", "<leader>gl", ":ToggleTerm cmd='golangci-lint run'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Go lint", buffer = true }))
      vim.keymap.set("n", "<leader>gc", ":ToggleTerm cmd='go clean -cache'<CR>", 
        vim.tbl_extend("force", opts, { desc = "Go clean cache", buffer = true }))
      
      -- Go install/get
      vim.keymap.set("n", "<leader>gi", function()
        local package = vim.fn.input("Package to install: ")
        if package ~= "" then
          vim.cmd("ToggleTerm cmd='go install " .. package .. "'")
        end
      end, vim.tbl_extend("force", opts, { desc = "Go install package", buffer = true }))
      
      vim.keymap.set("n", "<leader>gg", function()
        local package = vim.fn.input("Package to get: ")
        if package ~= "" then
          vim.cmd("ToggleTerm cmd='go get " .. package .. "'")
        end
      end, vim.tbl_extend("force", opts, { desc = "Go get package", buffer = true }))
      
      -- Go-specific LSP actions
      vim.keymap.set("n", "<leader>go", function()
        vim.lsp.buf.code_action({
          filter = function(action)
            return action.title:match("Organize Imports")
          end,
          apply = true,
        })
      end, vim.tbl_extend("force", opts, { desc = "Organize imports", buffer = true }))
      
      vim.keymap.set("n", "<leader>gI", function()
        vim.lsp.buf.code_action({
          filter = function(action)
            return action.title:match("Add missing imports")
          end,
          apply = true,
        })
      end, vim.tbl_extend("force", opts, { desc = "Add missing imports", buffer = true }))
      
      -- Check if go.nvim is available for enhanced features
      local has_go_nvim = pcall(require, "go")
      if has_go_nvim then
        -- go.nvim specific keymaps
        vim.keymap.set("n", "<leader>gta", ":GoAddTest<CR>", 
          vim.tbl_extend("force", opts, { desc = "Add test for function", buffer = true }))
        vim.keymap.set("n", "<leader>gts", ":GoAddExpTest<CR>", 
          vim.tbl_extend("force", opts, { desc = "Add exported test", buffer = true }))
        vim.keymap.set("n", "<leader>gtf", ":GoTestFile<CR>", 
          vim.tbl_extend("force", opts, { desc = "Test current file", buffer = true }))
        vim.keymap.set("n", "<leader>gtp", ":GoTestPkg<CR>", 
          vim.tbl_extend("force", opts, { desc = "Test current package", buffer = true }))
        
        -- Code generation
        vim.keymap.set("n", "<leader>gsj", ":GoAddTag json<CR>", 
          vim.tbl_extend("force", opts, { desc = "Add json tags", buffer = true }))
        vim.keymap.set("n", "<leader>gsy", ":GoAddTag yaml<CR>", 
          vim.tbl_extend("force", opts, { desc = "Add yaml tags", buffer = true }))
        vim.keymap.set("n", "<leader>gsr", ":GoRmTag<CR>", 
          vim.tbl_extend("force", opts, { desc = "Remove tags", buffer = true }))
        
        vim.keymap.set("n", "<leader>gsf", ":GoFillStruct<CR>", 
          vim.tbl_extend("force", opts, { desc = "Fill struct", buffer = true }))
        vim.keymap.set("n", "<leader>gse", ":GoFillSwitch<CR>", 
          vim.tbl_extend("force", opts, { desc = "Fill switch", buffer = true }))
        vim.keymap.set("n", "<leader>gsi", ":GoIfErr<CR>", 
          vim.tbl_extend("force", opts, { desc = "Add if err", buffer = true }))
        
        -- Code inspection
        vim.keymap.set("n", "<leader>gca", ":GoCoverage<CR>", 
          vim.tbl_extend("force", opts, { desc = "Show coverage", buffer = true }))
        vim.keymap.set("n", "<leader>gct", ":GoCoverageToggle<CR>", 
          vim.tbl_extend("force", opts, { desc = "Toggle coverage", buffer = true }))
      end
    end
  })
end

return M