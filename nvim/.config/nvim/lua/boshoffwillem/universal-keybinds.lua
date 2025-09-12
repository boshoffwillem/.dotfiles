local M = {}

-- Universal keybindings that should be consistent across all languages and frameworks
function M.setup_lsp_keybinds(client, bufnr)
  local nmap = function(keys, func, desc)
    if desc then
      desc = "LSP: " .. desc
    end
    vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc })
  end

  -- Core navigation (these should NEVER be overridden by language-specific modules)
  nmap("gd", vim.lsp.buf.definition, "[G]oto [D]efinition")
  nmap("gi", vim.lsp.buf.implementation, "[G]oto [I]mplementation")  
  nmap("gr", vim.lsp.buf.references, "[G]oto [R]eferences")
  nmap("gD", vim.lsp.buf.declaration, "[G]oto [D]eclaration")
  
  -- Code actions and refactoring (universal across all languages)
  nmap("<leader>la", vim.lsp.buf.code_action, "[L]SP Code [A]ctions")
  nmap("<leader>lr", vim.lsp.buf.rename, "[L]SP [R]ename")
  
  -- Additional universal LSP functions
  nmap("K", vim.lsp.buf.hover, "Hover Documentation")
  nmap("<C-k>", vim.lsp.buf.signature_help, "Signature Help")
  nmap("<leader>D", vim.lsp.buf.type_definition, "Type [D]efinition")
  nmap("<leader>l=", function()
    vim.lsp.buf.format({ async = true })
  end, "[L]SP [F]ormat")
  
  -- Workspace management
  nmap("<space>wa", vim.lsp.buf.add_workspace_folder, "[W]orkspace [A]dd Folder")
  nmap("<space>wr", vim.lsp.buf.remove_workspace_folder, "[W]orkspace [R]emove Folder")
  nmap("<space>wl", function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, "[W]orkspace [L]ist Folders")
  
  -- Symbols
  nmap("<leader>pt", require("telescope.builtin").lsp_dynamic_workspace_symbols, "[P]roject [T]elescope Symbols")

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, "Format", function(_)
    vim.lsp.buf.format()
  end, { desc = "Format current buffer with LSP" })
end

-- Universal test keybindings - these should work across all languages
function M.setup_test_keybinds()
  local opts = { noremap = true, silent = true }
  
  -- Universal test commands that work with any language/framework
  vim.keymap.set("n", "<leader>ctt", function()
    M.run_current_test()
  end, vim.tbl_extend("force", opts, { desc = "Run current test" }))
  
  vim.keymap.set("n", "<leader>cta", function()
    M.run_all_tests()
  end, vim.tbl_extend("force", opts, { desc = "Run all tests" }))
end

-- Intelligent test runner that detects available test frameworks and uses the best one
function M.run_current_test()
  -- Try neotest first (most advanced)
  local has_neotest, neotest = pcall(require, "neotest")
  if has_neotest then
    neotest.run.run()
    return
  end
  
  -- Fall back to language-specific test runners
  local filetype = vim.bo.filetype
  
  if filetype == "cs" then
    -- C# - use dotnet-utils
    local has_dotnet_utils, dotnet_utils = pcall(require, "boshoffwillem.dotnet-utils")
    if has_dotnet_utils then
      dotnet_utils.run_current_test()
      return
    end
  elseif filetype == "javascript" or filetype == "typescript" or filetype == "vue" then
    -- JavaScript/TypeScript/Vue - try common test commands
    vim.cmd('ToggleTerm cmd="npm test"')
    return
  elseif filetype == "python" then
    -- Python - try pytest
    vim.cmd('ToggleTerm cmd="pytest -v"')
    return
  elseif filetype == "go" then
    -- Go - use go test
    vim.cmd('ToggleTerm cmd="go test -v ./..."')
    return
  elseif filetype == "rust" then
    -- Rust - use cargo test
    vim.cmd('ToggleTerm cmd="cargo test"')
    return
  elseif filetype == "kotlin" then
    -- Kotlin - use gradle test
    local gradlew = vim.fn.getcwd() .. "/gradlew"
    if vim.fn.executable(gradlew) == 1 then
      vim.cmd('ToggleTerm cmd="./gradlew test"')
    else
      vim.cmd('ToggleTerm cmd="gradle test"')
    end
    return
  elseif filetype == "java" then
    -- Java - use gradle/maven test
    local gradlew = vim.fn.getcwd() .. "/gradlew"
    local mvnw = vim.fn.getcwd() .. "/mvnw"
    if vim.fn.executable(gradlew) == 1 then
      vim.cmd('ToggleTerm cmd="./gradlew test"')
    elseif vim.fn.executable(mvnw) == 1 then
      vim.cmd('ToggleTerm cmd="./mvnw test"')
    elseif vim.fn.filereadable(vim.fn.getcwd() .. "/pom.xml") == 1 then
      vim.cmd('ToggleTerm cmd="mvn test"')
    else
      vim.cmd('ToggleTerm cmd="gradle test"')
    end
    return
  elseif filetype == "swift" then
    -- Swift - use xcodebuild or swift test
    if vim.fn.filereadable("Package.swift") == 1 then
      vim.cmd('ToggleTerm cmd="swift test"')
    else
      local project = vim.fn.glob(vim.fn.getcwd() .. "/*.xcworkspace")
      if project == "" then
        project = vim.fn.glob(vim.fn.getcwd() .. "/*.xcodeproj")
      end
      if project ~= "" then
        vim.cmd('ToggleTerm cmd="xcodebuild test -project ' .. project .. '"')
      else
        vim.cmd('ToggleTerm cmd="swift test"')
      end
    end
    return
  end
  
  vim.notify("No test runner found for filetype: " .. filetype, vim.log.levels.WARN)
end

-- Intelligent test runner for all tests
function M.run_all_tests()
  -- Try neotest first (most advanced)
  local has_neotest, neotest = pcall(require, "neotest")
  if has_neotest then
    neotest.run.run(vim.fn.getcwd())
    return
  end
  
  -- Fall back to language-specific test runners
  local filetype = vim.bo.filetype
  
  if filetype == "cs" then
    -- C# - use dotnet-utils
    local has_dotnet_utils, dotnet_utils = pcall(require, "boshoffwillem.dotnet-utils")
    if has_dotnet_utils then
      dotnet_utils.run_tests()
      return
    end
  elseif filetype == "javascript" or filetype == "typescript" or filetype == "vue" then
    -- JavaScript/TypeScript/Vue - try common test commands
    vim.cmd('ToggleTerm cmd="npm test"')
    return
  elseif filetype == "python" then
    -- Python - try pytest
    vim.cmd('ToggleTerm cmd="pytest"')
    return
  elseif filetype == "go" then
    -- Go - use go test
    vim.cmd('ToggleTerm cmd="go test ./..."')
    return
  elseif filetype == "rust" then
    -- Rust - use cargo test
    vim.cmd('ToggleTerm cmd="cargo test"')
    return
  elseif filetype == "kotlin" then
    -- Kotlin - use gradle test
    local gradlew = vim.fn.getcwd() .. "/gradlew"
    if vim.fn.executable(gradlew) == 1 then
      vim.cmd('ToggleTerm cmd="./gradlew test"')
    else
      vim.cmd('ToggleTerm cmd="gradle test"')
    end
    return
  elseif filetype == "java" then
    -- Java - use gradle/maven test
    local gradlew = vim.fn.getcwd() .. "/gradlew"
    local mvnw = vim.fn.getcwd() .. "/mvnw"
    if vim.fn.executable(gradlew) == 1 then
      vim.cmd('ToggleTerm cmd="./gradlew test"')
    elseif vim.fn.executable(mvnw) == 1 then
      vim.cmd('ToggleTerm cmd="./mvnw test"')
    elseif vim.fn.filereadable(vim.fn.getcwd() .. "/pom.xml") == 1 then
      vim.cmd('ToggleTerm cmd="mvn test"')
    else
      vim.cmd('ToggleTerm cmd="gradle test"')
    end
    return
  elseif filetype == "swift" then
    -- Swift - use xcodebuild or swift test
    if vim.fn.filereadable("Package.swift") == 1 then
      vim.cmd('ToggleTerm cmd="swift test"')
    else
      local project = vim.fn.glob(vim.fn.getcwd() .. "/*.xcworkspace")
      if project == "" then
        project = vim.fn.glob(vim.fn.getcwd() .. "/*.xcodeproj")
      end
      if project ~= "" then
        vim.cmd('ToggleTerm cmd="xcodebuild test -project ' .. project .. '"')
      else
        vim.cmd('ToggleTerm cmd="swift test"')
      end
    end
    return
  end
  
  vim.notify("No test runner found for filetype: " .. filetype, vim.log.levels.WARN)
end

-- Setup function to initialize all universal keybindings
function M.setup()
  M.setup_test_keybinds()
end

return M