-- Enhanced Java configuration for Android development
local status, jdtls = pcall(require, "jdtls")
if not status then
  return
end

-- Find root directory
local root_markers = { "gradlew", "mvnw", ".git", "pom.xml", "build.gradle", "settings.gradle" }
local root_dir = require("jdtls.setup").find_root(root_markers)

-- Workspace directory
local workspace_dir = vim.fn.stdpath("data") .. "/jdtls-workspace/" .. vim.fn.fnamemodify(root_dir, ":p:h:t")

-- Mason installation path
local mason_path = vim.fn.stdpath("data") .. "/mason"
local jdtls_path = mason_path .. "/packages/jdtls"

-- Find the launcher jar
local launcher = vim.fn.glob(jdtls_path .. "/plugins/org.eclipse.equinox.launcher_*.jar")

-- OS specific config
local config_dir = "config_linux"
if vim.fn.has("mac") == 1 then
  config_dir = "config_mac"
elseif vim.fn.has("win32") == 1 then
  config_dir = "config_win"
end

-- Android SDK paths
local android_home = os.getenv("ANDROID_HOME") or os.getenv("ANDROID_SDK_ROOT")
if not android_home and vim.fn.has("mac") == 1 then
  android_home = vim.fn.expand("~/Library/Android/sdk")
elseif not android_home and vim.fn.has("unix") == 1 then
  android_home = vim.fn.expand("~/Android/Sdk")
end

local config = {
  cmd = {
    "java",
    "-Declipse.application=org.eclipse.jdt.ls.core.id1",
    "-Dosgi.bundles.defaultStartLevel=4",
    "-Declipse.product=org.eclipse.jdt.ls.core.product",
    "-Dlog.protocol=true",
    "-Dlog.level=ALL",
    "-Xmx1g",
    "--add-modules=ALL-SYSTEM",
    "--add-opens", "java.base/java.util=ALL-UNNAMED",
    "--add-opens", "java.base/java.lang=ALL-UNNAMED",
    "-jar", launcher,
    "-configuration", jdtls_path .. "/" .. config_dir,
    "-data", workspace_dir,
  },
  
  root_dir = root_dir,
  
  settings = {
    java = {
      eclipse = {
        downloadSources = true,
      },
      configuration = {
        updateBuildConfiguration = "interactive",
        -- Android SDK paths
        runtimes = android_home and {
          {
            name = "JavaSE-11",
            path = android_home .. "/jre",
          },
          {
            name = "JavaSE-17",
            path = "/usr/lib/jvm/java-17-openjdk",
          },
        } or {},
      },
      maven = {
        downloadSources = true,
      },
      implementationsCodeLens = {
        enabled = true,
      },
      referencesCodeLens = {
        enabled = true,
      },
      references = {
        includeDecompiledSources = true,
      },
      format = {
        enabled = true,
        settings = {
          url = vim.fn.stdpath("config") .. "/java-google-style.xml",
          profile = "GoogleStyle",
        },
      },
      signatureHelp = { enabled = true },
      completion = {
        favoriteStaticMembers = {
          "org.junit.jupiter.api.Assertions.*",
          "org.junit.Assert.*",
          "org.junit.Assume.*",
          "org.mockito.Mockito.*",
          "org.mockito.ArgumentMatchers.*",
          "android.util.Log.*",
        },
        importOrder = {
          "android",
          "androidx",
          "com",
          "java",
          "javax",
          "org",
        },
      },
      sources = {
        organizeImports = {
          starThreshold = 9999,
          staticStarThreshold = 9999,
        },
      },
      codeGeneration = {
        toString = {
          template = "${object.className}{${member.name()}=${member.value}, ${otherMembers}}",
        },
        useBlocks = true,
      },
    },
  },
  
  init_options = {
    bundles = {},
  },
  
  on_attach = function(client, bufnr)
    -- Set up universal LSP keybindings
    require("boshoffwillem.universal-keybinds").setup_lsp_keybinds(client, bufnr)
    
    -- Java specific keybindings
    local opts = { noremap = true, silent = true, buffer = bufnr }
    vim.keymap.set("n", "<leader>jo", jdtls.organize_imports, vim.tbl_extend("force", opts, { desc = "[J]ava [O]rganize Imports" }))
    vim.keymap.set("n", "<leader>jv", jdtls.extract_variable, vim.tbl_extend("force", opts, { desc = "[J]ava Extract [V]ariable" }))
    vim.keymap.set("n", "<leader>jc", jdtls.extract_constant, vim.tbl_extend("force", opts, { desc = "[J]ava Extract [C]onstant" }))
    vim.keymap.set("n", "<leader>jm", jdtls.extract_method, vim.tbl_extend("force", opts, { desc = "[J]ava Extract [M]ethod" }))
    vim.keymap.set("v", "<leader>jv", function() jdtls.extract_variable(true) end, vim.tbl_extend("force", opts, { desc = "[J]ava Extract [V]ariable" }))
    vim.keymap.set("v", "<leader>jc", function() jdtls.extract_constant(true) end, vim.tbl_extend("force", opts, { desc = "[J]ava Extract [C]onstant" }))
    vim.keymap.set("v", "<leader>jm", function() jdtls.extract_method(true) end, vim.tbl_extend("force", opts, { desc = "[J]ava Extract [M]ethod" }))
    
    -- Enable jdtls commands
    require("jdtls").setup_dap({ hotcodereplace = "auto" })
    require("jdtls.dap").setup_dap_main_class_configs()
  end,
  
  capabilities = require("cmp_nvim_lsp").default_capabilities(),
  flags = {
    debounce_text_changes = 150,
  },
}

-- Start or attach jdtls
jdtls.start_or_attach(config)