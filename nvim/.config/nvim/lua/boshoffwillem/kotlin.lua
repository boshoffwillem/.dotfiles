local M = {}

-- Android SDK configuration
local function get_android_home()
  local android_home = os.getenv("ANDROID_HOME") or os.getenv("ANDROID_SDK_ROOT")
  if not android_home then
    -- Common default locations
    if vim.fn.has("mac") == 1 then
      android_home = vim.fn.expand("~/Library/Android/sdk")
    elseif vim.fn.has("unix") == 1 then
      android_home = vim.fn.expand("~/Android/Sdk")
    end
  end
  return android_home
end

-- Find gradle wrapper in project
local function find_gradlew()
  local gradlew = vim.fn.getcwd() .. "/gradlew"
  if vim.fn.executable(gradlew) == 1 then
    return gradlew
  end
  -- Try parent directories
  local parent = vim.fn.fnamemodify(vim.fn.getcwd(), ":h")
  gradlew = parent .. "/gradlew"
  if vim.fn.executable(gradlew) == 1 then
    return gradlew
  end
  return "gradle" -- fallback to system gradle
end

function M.setup()
  -- Set Android environment variables if not set
  local android_home = get_android_home()
  if android_home and vim.fn.isdirectory(android_home) == 1 then
    vim.env.ANDROID_HOME = android_home
    vim.env.ANDROID_SDK_ROOT = android_home
    
    -- Add Android tools to PATH
    local path_sep = vim.fn.has("win32") == 1 and ";" or ":"
    local android_paths = {
      android_home .. "/platform-tools",
      android_home .. "/tools",
      android_home .. "/tools/bin",
      android_home .. "/emulator"
    }
    
    for _, path in ipairs(android_paths) do
      if vim.fn.isdirectory(path) == 1 then
        vim.env.PATH = path .. path_sep .. vim.env.PATH
      end
    end
  end
end

function M.setup_keymaps()
  local opts = { noremap = true, silent = true }
  
  -- Gradle commands
  vim.keymap.set("n", "<leader>kb", function()
    M.gradle_build()
  end, vim.tbl_extend("force", opts, { desc = "[K]otlin [B]uild project" }))
  
  vim.keymap.set("n", "<leader>kc", function()
    M.gradle_clean()
  end, vim.tbl_extend("force", opts, { desc = "[K]otlin [C]lean project" }))
  
  vim.keymap.set("n", "<leader>kr", function()
    M.gradle_run()
  end, vim.tbl_extend("force", opts, { desc = "[K]otlin [R]un app" }))
  
  vim.keymap.set("n", "<leader>kt", function()
    M.run_tests()
  end, vim.tbl_extend("force", opts, { desc = "[K]otlin run [T]ests" }))
  
  vim.keymap.set("n", "<leader>kT", function()
    M.run_current_test()
  end, vim.tbl_extend("force", opts, { desc = "[K]otlin run current [T]est" }))
  
  -- Android specific
  vim.keymap.set("n", "<leader>ki", function()
    M.install_apk()
  end, vim.tbl_extend("force", opts, { desc = "[K]otlin [I]nstall APK" }))
  
  vim.keymap.set("n", "<leader>kl", function()
    M.show_logcat()
  end, vim.tbl_extend("force", opts, { desc = "[K]otlin show [L]ogcat" }))
  
  vim.keymap.set("n", "<leader>kd", function()
    M.list_devices()
  end, vim.tbl_extend("force", opts, { desc = "[K]otlin list [D]evices" }))
  
  vim.keymap.set("n", "<leader>ke", function()
    M.launch_emulator()
  end, vim.tbl_extend("force", opts, { desc = "[K]otlin launch [E]mulator" }))
  
  vim.keymap.set("n", "<leader>ks", function()
    M.gradle_sync()
  end, vim.tbl_extend("force", opts, { desc = "[K]otlin [S]ync project" }))
  
  -- Add package/dependency
  vim.keymap.set("n", "<leader>ka", function()
    M.add_dependency()
  end, vim.tbl_extend("force", opts, { desc = "[K]otlin [A]dd dependency" }))
end

-- Gradle commands
function M.gradle_build()
  local gradlew = find_gradlew()
  local cmd = gradlew .. " build"
  require("toggleterm").exec(cmd, nil, nil, nil, "float")
end

function M.gradle_clean()
  local gradlew = find_gradlew()
  local cmd = gradlew .. " clean"
  require("toggleterm").exec(cmd, nil, nil, nil, "float")
end

function M.gradle_run()
  local gradlew = find_gradlew()
  local cmd = gradlew .. " installDebug && adb shell monkey -p $(grep applicationId app/build.gradle | awk '{print $2}' | tr -d '\"') -c android.intent.category.LAUNCHER 1"
  require("toggleterm").exec(cmd, nil, nil, nil, "float")
end

function M.gradle_sync()
  local gradlew = find_gradlew()
  local cmd = gradlew .. " --refresh-dependencies"
  require("toggleterm").exec(cmd, nil, nil, nil, "float")
end

-- Testing
function M.run_tests()
  local gradlew = find_gradlew()
  local cmd = gradlew .. " test"
  require("toggleterm").exec(cmd, nil, nil, nil, "float")
end

function M.run_current_test()
  local test_name = vim.fn.expand("<cword>")
  local gradlew = find_gradlew()
  local cmd = gradlew .. " test --tests '*." .. test_name .. "'"
  require("toggleterm").exec(cmd, nil, nil, nil, "float")
end

-- Android specific commands
function M.install_apk()
  local gradlew = find_gradlew()
  local cmd = gradlew .. " installDebug"
  require("toggleterm").exec(cmd, nil, nil, nil, "float")
end

function M.show_logcat()
  local cmd = "adb logcat"
  require("toggleterm").exec(cmd, nil, nil, nil, "float")
end

function M.list_devices()
  local cmd = "adb devices"
  require("toggleterm").exec(cmd, nil, nil, nil, "float")
end

function M.launch_emulator()
  -- List available emulators and let user choose
  vim.fn.system("emulator -list-avds > /tmp/emulators.txt")
  local emulators = vim.fn.readfile("/tmp/emulators.txt")
  
  if #emulators == 0 then
    vim.notify("No emulators found. Please create one using Android Studio AVD Manager.", vim.log.levels.WARN)
    return
  end
  
  vim.ui.select(emulators, {
    prompt = "Select emulator to launch:",
  }, function(choice)
    if choice then
      local cmd = "emulator -avd " .. choice .. " &"
      vim.fn.system(cmd)
      vim.notify("Launching emulator: " .. choice, vim.log.levels.INFO)
    end
  end)
end

-- Add dependency to build.gradle
function M.add_dependency()
  vim.ui.input({
    prompt = "Enter dependency (e.g., 'com.squareup.retrofit2:retrofit:2.9.0'): ",
  }, function(input)
    if input then
      -- Find the build.gradle file
      local build_gradle = vim.fn.getcwd() .. "/app/build.gradle"
      if vim.fn.filereadable(build_gradle) == 0 then
        build_gradle = vim.fn.getcwd() .. "/build.gradle"
      end
      
      if vim.fn.filereadable(build_gradle) == 1 then
        -- Read the file and add dependency
        local lines = vim.fn.readfile(build_gradle)
        local in_dependencies = false
        local insert_line = 0
        
        for i, line in ipairs(lines) do
          if line:match("dependencies%s*{") then
            in_dependencies = true
          elseif in_dependencies and line:match("}") then
            insert_line = i
            break
          end
        end
        
        if insert_line > 0 then
          table.insert(lines, insert_line, "    implementation '" .. input .. "'")
          vim.fn.writefile(lines, build_gradle)
          vim.notify("Added dependency: " .. input, vim.log.levels.INFO)
          
          -- Sync project
          M.gradle_sync()
        else
          vim.notify("Could not find dependencies block in build.gradle", vim.log.levels.ERROR)
        end
      else
        vim.notify("Could not find build.gradle file", vim.log.levels.ERROR)
      end
    end
  end)
end

-- Auto commands for Kotlin files
function M.setup_autocommands()
  local group = vim.api.nvim_create_augroup("KotlinAndroid", { clear = true })
  
  -- Auto-import on save
  vim.api.nvim_create_autocmd("BufWritePre", {
    group = group,
    pattern = "*.kt",
    callback = function()
      vim.lsp.buf.code_action({
        context = {
          only = { "source.organizeImports" },
        },
        apply = true,
      })
    end,
  })
  
  -- Set makeprg for Kotlin files
  vim.api.nvim_create_autocmd("FileType", {
    group = group,
    pattern = "kotlin",
    callback = function()
      vim.opt_local.makeprg = find_gradlew() .. " build"
      vim.opt_local.errorformat = "%f:%l:%c: %t%*[^:]: %m"
    end,
  })
end

return M