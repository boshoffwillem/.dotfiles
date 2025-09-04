local M = {}

-- Xcode configuration
local function get_xcode_path()
  local handle = io.popen("xcode-select -p 2>/dev/null")
  if handle then
    local result = handle:read("*a")
    handle:close()
    return result:gsub("%s+$", "") -- trim whitespace
  end
  return nil
end

-- Find xcodeproj or xcworkspace in project
local function find_xcode_project()
  local cwd = vim.fn.getcwd()
  
  -- First look for workspace
  local workspace = vim.fn.glob(cwd .. "/*.xcworkspace")
  if workspace ~= "" then
    return workspace
  end
  
  -- Then look for project
  local project = vim.fn.glob(cwd .. "/*.xcodeproj")
  if project ~= "" then
    return project
  end
  
  -- Try parent directory
  local parent = vim.fn.fnamemodify(cwd, ":h")
  workspace = vim.fn.glob(parent .. "/*.xcworkspace")
  if workspace ~= "" then
    return workspace
  end
  
  project = vim.fn.glob(parent .. "/*.xcodeproj")
  if project ~= "" then
    return project
  end
  
  return nil
end

-- Get available iOS simulators
local function get_simulators()
  local handle = io.popen("xcrun simctl list devices available -j 2>/dev/null")
  if handle then
    local result = handle:read("*a")
    handle:close()
    
    -- Parse JSON to get simulator names
    local ok, json = pcall(vim.fn.json_decode, result)
    if ok and json.devices then
      local simulators = {}
      for runtime, devices in pairs(json.devices) do
        if runtime:match("iOS") then
          for _, device in ipairs(devices) do
            if device.state == "Booted" or device.isAvailable then
              table.insert(simulators, {
                name = device.name,
                udid = device.udid,
                runtime = runtime,
                booted = device.state == "Booted"
              })
            end
          end
        end
      end
      return simulators
    end
  end
  return {}
end

function M.setup()
  -- Verify Xcode Command Line Tools are installed
  local xcode_path = get_xcode_path()
  if not xcode_path then
    vim.notify("Xcode Command Line Tools not found. Please install via: xcode-select --install", vim.log.levels.WARN)
    return
  end
  
  -- Set up sourcekit-lsp path
  local sourcekit_lsp = xcode_path .. "/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"
  if vim.fn.executable(sourcekit_lsp) == 0 then
    vim.notify("sourcekit-lsp not found. Please ensure Xcode is properly installed.", vim.log.levels.WARN)
  end
end

function M.setup_keymaps()
  local opts = { noremap = true, silent = true }
  
  -- Xcode build commands
  vim.keymap.set("n", "<leader>sb", function()
    M.xcode_build()
  end, vim.tbl_extend("force", opts, { desc = "[S]wift [B]uild project" }))
  
  vim.keymap.set("n", "<leader>sc", function()
    M.xcode_clean()
  end, vim.tbl_extend("force", opts, { desc = "[S]wift [C]lean project" }))
  
  vim.keymap.set("n", "<leader>sr", function()
    M.xcode_run()
  end, vim.tbl_extend("force", opts, { desc = "[S]wift [R]un app" }))
  
  vim.keymap.set("n", "<leader>st", function()
    M.run_tests()
  end, vim.tbl_extend("force", opts, { desc = "[S]wift run [T]ests" }))
  
  vim.keymap.set("n", "<leader>sT", function()
    M.run_current_test()
  end, vim.tbl_extend("force", opts, { desc = "[S]wift run current [T]est" }))
  
  -- iOS specific
  vim.keymap.set("n", "<leader>si", function()
    M.install_app()
  end, vim.tbl_extend("force", opts, { desc = "[S]wift [I]nstall app" }))
  
  vim.keymap.set("n", "<leader>sl", function()
    M.show_device_logs()
  end, vim.tbl_extend("force", opts, { desc = "[S]wift show [L]ogs" }))
  
  vim.keymap.set("n", "<leader>sd", function()
    M.list_simulators()
  end, vim.tbl_extend("force", opts, { desc = "[S]wift list [D]evices/Simulators" }))
  
  vim.keymap.set("n", "<leader>se", function()
    M.launch_simulator()
  end, vim.tbl_extend("force", opts, { desc = "[S]wift launch simulator ([E]mulator)" }))
  
  vim.keymap.set("n", "<leader>sa", function()
    M.archive_project()
  end, vim.tbl_extend("force", opts, { desc = "[S]wift [A]rchive project" }))
  
  -- Swift Package Manager
  vim.keymap.set("n", "<leader>sp", function()
    M.spm_resolve()
  end, vim.tbl_extend("force", opts, { desc = "[S]wift [P]ackage resolve" }))
  
  vim.keymap.set("n", "<leader>su", function()
    M.spm_update()
  end, vim.tbl_extend("force", opts, { desc = "[S]wift package [U]pdate" }))
  
  vim.keymap.set("n", "<leader>sP", function()
    M.add_package()
  end, vim.tbl_extend("force", opts, { desc = "[S]wift add [P]ackage" }))
  
  -- SwiftUI Preview
  vim.keymap.set("n", "<leader>sv", function()
    M.toggle_preview()
  end, vim.tbl_extend("force", opts, { desc = "[S]wift toggle pre[V]iew" }))
end

-- Xcode build commands
function M.xcode_build()
  local project = find_xcode_project()
  if not project then
    vim.notify("No Xcode project found", vim.log.levels.ERROR)
    return
  end
  
  local cmd
  if project:match("%.xcworkspace$") then
    cmd = "xcodebuild -workspace " .. vim.fn.shellescape(project) .. " -scheme $(xcodebuild -list -json | jq -r '.workspace.schemes[0]') build"
  else
    cmd = "xcodebuild -project " .. vim.fn.shellescape(project) .. " build"
  end
  
  require("toggleterm").exec(cmd, nil, nil, nil, "float")
end

function M.xcode_clean()
  local project = find_xcode_project()
  if not project then
    -- Try Swift Package Manager
    if vim.fn.filereadable("Package.swift") == 1 then
      require("toggleterm").exec("swift package clean", nil, nil, nil, "float")
      return
    end
    vim.notify("No Xcode project or Package.swift found", vim.log.levels.ERROR)
    return
  end
  
  local cmd
  if project:match("%.xcworkspace$") then
    cmd = "xcodebuild -workspace " .. vim.fn.shellescape(project) .. " -scheme $(xcodebuild -list -json | jq -r '.workspace.schemes[0]') clean"
  else
    cmd = "xcodebuild -project " .. vim.fn.shellescape(project) .. " clean"
  end
  
  require("toggleterm").exec(cmd, nil, nil, nil, "float")
end

function M.xcode_run()
  local simulators = get_simulators()
  local booted_sim = nil
  
  -- Find a booted simulator
  for _, sim in ipairs(simulators) do
    if sim.booted then
      booted_sim = sim
      break
    end
  end
  
  if not booted_sim and #simulators > 0 then
    -- Boot the first available simulator
    booted_sim = simulators[1]
    vim.fn.system("xcrun simctl boot " .. booted_sim.udid)
  end
  
  if not booted_sim then
    vim.notify("No iOS simulators available", vim.log.levels.ERROR)
    return
  end
  
  local project = find_xcode_project()
  if not project then
    vim.notify("No Xcode project found", vim.log.levels.ERROR)
    return
  end
  
  local cmd
  if project:match("%.xcworkspace$") then
    cmd = "xcodebuild -workspace " .. vim.fn.shellescape(project) .. 
          " -scheme $(xcodebuild -list -json | jq -r '.workspace.schemes[0]')" ..
          " -destination 'id=" .. booted_sim.udid .. "'" ..
          " build run"
  else
    cmd = "xcodebuild -project " .. vim.fn.shellescape(project) .. 
          " -destination 'id=" .. booted_sim.udid .. "'" ..
          " build run"
  end
  
  require("toggleterm").exec(cmd, nil, nil, nil, "float")
end

-- Testing
function M.run_tests()
  local project = find_xcode_project()
  if not project then
    -- Try Swift Package Manager
    if vim.fn.filereadable("Package.swift") == 1 then
      require("toggleterm").exec("swift test", nil, nil, nil, "float")
      return
    end
    vim.notify("No Xcode project or Package.swift found", vim.log.levels.ERROR)
    return
  end
  
  local cmd
  if project:match("%.xcworkspace$") then
    cmd = "xcodebuild test -workspace " .. vim.fn.shellescape(project) .. 
          " -scheme $(xcodebuild -list -json | jq -r '.workspace.schemes[0]')" ..
          " -destination 'platform=iOS Simulator,name=iPhone 15'"
  else
    cmd = "xcodebuild test -project " .. vim.fn.shellescape(project) ..
          " -destination 'platform=iOS Simulator,name=iPhone 15'"
  end
  
  require("toggleterm").exec(cmd, nil, nil, nil, "float")
end

function M.run_current_test()
  local test_name = vim.fn.expand("<cword>")
  local class_name = vim.fn.expand("%:t:r") -- filename without extension
  
  local project = find_xcode_project()
  if not project then
    vim.notify("No Xcode project found", vim.log.levels.ERROR)
    return
  end
  
  local cmd
  if project:match("%.xcworkspace$") then
    cmd = "xcodebuild test -workspace " .. vim.fn.shellescape(project) ..
          " -scheme $(xcodebuild -list -json | jq -r '.workspace.schemes[0]')" ..
          " -destination 'platform=iOS Simulator,name=iPhone 15'" ..
          " -only-testing:" .. class_name .. "/" .. test_name
  else
    cmd = "xcodebuild test -project " .. vim.fn.shellescape(project) ..
          " -destination 'platform=iOS Simulator,name=iPhone 15'" ..
          " -only-testing:" .. class_name .. "/" .. test_name
  end
  
  require("toggleterm").exec(cmd, nil, nil, nil, "float")
end

-- iOS specific commands
function M.install_app()
  local project = find_xcode_project()
  if not project then
    vim.notify("No Xcode project found", vim.log.levels.ERROR)
    return
  end
  
  local simulators = get_simulators()
  if #simulators == 0 then
    vim.notify("No iOS simulators available", vim.log.levels.ERROR)
    return
  end
  
  vim.ui.select(vim.tbl_map(function(s) return s.name end, simulators), {
    prompt = "Select simulator to install on:",
  }, function(choice, idx)
    if choice then
      local sim = simulators[idx]
      local cmd
      if project:match("%.xcworkspace$") then
        cmd = "xcodebuild -workspace " .. vim.fn.shellescape(project) ..
              " -scheme $(xcodebuild -list -json | jq -r '.workspace.schemes[0]')" ..
              " -destination 'id=" .. sim.udid .. "' install"
      else
        cmd = "xcodebuild -project " .. vim.fn.shellescape(project) ..
              " -destination 'id=" .. sim.udid .. "' install"
      end
      require("toggleterm").exec(cmd, nil, nil, nil, "float")
    end
  end)
end

function M.show_device_logs()
  local cmd = "xcrun simctl spawn booted log stream --level debug"
  require("toggleterm").exec(cmd, nil, nil, nil, "float")
end

function M.list_simulators()
  local simulators = get_simulators()
  if #simulators == 0 then
    vim.notify("No iOS simulators available", vim.log.levels.WARN)
    return
  end
  
  local lines = {"Available iOS Simulators:"}
  for _, sim in ipairs(simulators) do
    local status = sim.booted and " (Booted)" or ""
    table.insert(lines, string.format("  %s - %s%s", sim.name, sim.udid:sub(1, 8), status))
  end
  
  vim.notify(table.concat(lines, "\n"), vim.log.levels.INFO)
end

function M.launch_simulator()
  local simulators = get_simulators()
  if #simulators == 0 then
    vim.notify("No iOS simulators available", vim.log.levels.ERROR)
    return
  end
  
  vim.ui.select(vim.tbl_map(function(s) 
    return s.name .. (s.booted and " (Booted)" or "")
  end, simulators), {
    prompt = "Select simulator to launch:",
  }, function(choice, idx)
    if choice then
      local sim = simulators[idx]
      if not sim.booted then
        vim.fn.system("xcrun simctl boot " .. sim.udid)
      end
      vim.fn.system("open -a Simulator")
      vim.notify("Launched simulator: " .. sim.name, vim.log.levels.INFO)
    end
  end)
end

function M.archive_project()
  local project = find_xcode_project()
  if not project then
    vim.notify("No Xcode project found", vim.log.levels.ERROR)
    return
  end
  
  local archive_path = vim.fn.expand("~/Desktop/") .. os.date("%Y%m%d_%H%M%S") .. ".xcarchive"
  local cmd
  if project:match("%.xcworkspace$") then
    cmd = "xcodebuild archive -workspace " .. vim.fn.shellescape(project) ..
          " -scheme $(xcodebuild -list -json | jq -r '.workspace.schemes[0]')" ..
          " -archivePath " .. archive_path
  else
    cmd = "xcodebuild archive -project " .. vim.fn.shellescape(project) ..
          " -archivePath " .. archive_path
  end
  
  require("toggleterm").exec(cmd, nil, nil, nil, "float")
end

-- Swift Package Manager commands
function M.spm_resolve()
  if vim.fn.filereadable("Package.swift") == 0 then
    vim.notify("No Package.swift found", vim.log.levels.ERROR)
    return
  end
  
  require("toggleterm").exec("swift package resolve", nil, nil, nil, "float")
end

function M.spm_update()
  if vim.fn.filereadable("Package.swift") == 0 then
    vim.notify("No Package.swift found", vim.log.levels.ERROR)
    return
  end
  
  require("toggleterm").exec("swift package update", nil, nil, nil, "float")
end

function M.add_package()
  vim.ui.input({
    prompt = "Enter package URL (e.g., https://github.com/Alamofire/Alamofire.git): ",
  }, function(url)
    if url then
      vim.ui.input({
        prompt = "Enter version requirement (e.g., 5.0.0, branch:main, or leave empty for latest): ",
      }, function(version)
        local cmd = "swift package add " .. url
        if version and version ~= "" then
          if version:match("^branch:") then
            cmd = cmd .. " --branch " .. version:sub(8)
          else
            cmd = cmd .. " --from " .. version
          end
        end
        require("toggleterm").exec(cmd, nil, nil, nil, "float")
      end)
    end
  end)
end

-- SwiftUI Preview (placeholder - requires more complex integration)
function M.toggle_preview()
  vim.notify("SwiftUI Preview requires Xcode. Use Xcode for live previews.", vim.log.levels.INFO)
  -- Could potentially integrate with xcodebuild's preview capabilities in future
end

-- Auto commands for Swift files
function M.setup_autocommands()
  local group = vim.api.nvim_create_augroup("SwiftiOS", { clear = true })
  
  -- Format on save using swift-format
  vim.api.nvim_create_autocmd("BufWritePre", {
    group = group,
    pattern = "*.swift",
    callback = function()
      -- Only format if swift-format is available
      if vim.fn.executable("swift-format") == 1 then
        vim.lsp.buf.format({ async = false })
      end
    end,
  })
  
  -- Set makeprg for Swift files
  vim.api.nvim_create_autocmd("FileType", {
    group = group,
    pattern = "swift",
    callback = function()
      if vim.fn.filereadable("Package.swift") == 1 then
        vim.opt_local.makeprg = "swift build"
      else
        local project = find_xcode_project()
        if project then
          if project:match("%.xcworkspace$") then
            vim.opt_local.makeprg = "xcodebuild -workspace " .. vim.fn.shellescape(project) .. " build"
          else
            vim.opt_local.makeprg = "xcodebuild -project " .. vim.fn.shellescape(project) .. " build"
          end
        end
      end
      vim.opt_local.errorformat = "%f:%l:%c: %t%*[^:]: %m"
    end,
  })
end

return M