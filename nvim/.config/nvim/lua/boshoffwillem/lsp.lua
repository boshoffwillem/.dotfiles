-- Mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
vim.keymap.set("n", "<space>e", vim.diagnostic.open_float)
vim.keymap.set("n", "[d", vim.diagnostic.goto_prev)
vim.keymap.set("n", "]d", vim.diagnostic.goto_next)
vim.keymap.set("n", "<space>q", vim.diagnostic.setloclist)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  -- Set up universal LSP keybindings first
  require("boshoffwillem.universal-keybinds").setup_lsp_keybinds(client, bufnr)

  local nmap = function(keys, func, desc)
    if desc then
      desc = "LSP: " .. desc
    end

    vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc })
  end

  if client.name == "omnisharp" or client.name == "roslyn" then
    -- https://github.com/OmniSharp/omnisharp-roslyn/issues/2483#issuecomment-1492605642
    if client.server_capabilities.semanticTokensProvider and client.server_capabilities.semanticTokensProvider.legend then
      local tokenModifiers = client.server_capabilities.semanticTokensProvider.legend.tokenModifiers
      for i, v in ipairs(tokenModifiers) do
        local tmp = string.gsub(v, " ", "_")
        tokenModifiers[i] = string.gsub(tmp, "-_", "")
      end
      local tokenTypes = client.server_capabilities.semanticTokensProvider.legend.tokenTypes
      for i, v in ipairs(tokenTypes) do
        local tmp = string.gsub(v, " ", "_")
        tokenTypes[i] = string.gsub(tmp, "-_", "")
      end
    end

    -- C# specific keymaps
    nmap("<leader>cc", function()
      vim.lsp.buf.code_action({
        filter = function(action)
          return action.isPreferred
        end,
        apply = true,
      })
    end, "Apply preferred code action")

    nmap("<leader>cf", function()
      vim.lsp.buf.code_action({
        filter = function(action)
          return action.title:match("Fix all occurrences")
        end,
        apply = true,
      })
    end, "Fix all occurrences")

    nmap("<leader>cg", function()
      vim.lsp.buf.code_action({
        filter = function(action)
          return action.title:match("Generate")
        end,
      })
    end, "Generate code")

    nmap("<leader>cr", function()
      vim.lsp.buf.code_action({
        filter = function(action)
          return action.title:match("Refactor")
        end,
      })
    end, "Refactor")
  end

  -- Universal keybindings are handled by universal-keybinds.lua
  -- Only language-specific keybindings should be added here
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)
capabilities.textDocument.foldingRange = {
  dynamicRegistration = false,
  lineFoldingOnly = true
}

local servers = {
  angularls = {
    cmd = { "ngserver", "--stdio", "--tsProbeLocations", ".", "--ngProbeLocations", "." },
    on_new_config = function(new_config, new_root_dir)
      new_config.cmd = { "ngserver", "--stdio", "--tsProbeLocations", new_root_dir, "--ngProbeLocations", new_root_dir }
    end,
  },
  azure_pipelines_ls = {
    filetypes = { "yaml.azure", "yaml.azurepipelines" },
    root_patterns = { ".azure-pipelines", "azure-pipelines.yml", "azure-pipelines.yaml" },
    settings = {
      yaml = {
        schemaStore = {
          enable = false,
          url = "",
        },
        schemas = {
          ["https://raw.githubusercontent.com/microsoft/azure-pipelines-vscode/master/service-schema.json"] = {
            "azure-pipelines*.yml",
            "azure-pipelines*.yaml",
            ".azure-pipelines/**/*.yml",
            ".azure-pipelines/**/*.yaml",
          },
        },
      },
    },
  },
  dockerls = {},
  docker_compose_language_service = {},
  -- fsautocomplete = {},
  gopls = {
    cmd = { "gopls" },
    filetypes = { "go", "gomod", "gowork", "gotmpl" },
    root_dir = require("lspconfig.util").root_pattern("go.work", "go.mod", ".git"),
    settings = {
      gopls = {
        completeUnimported = true,
        usePlaceholders = true,
        analyses = {
          unusedparams = true,
        },
        codelenses = {
          gc_details = false,
          generate = true,
          regenerate_cgo = true,
          run_govulncheck = true,
          test = true,
          tidy = true,
          upgrade_dependency = true,
          vendor = true,
        },
        hints = {
          assignVariableTypes = true,
          compositeLiteralFields = true,
          compositeLiteralTypes = true,
          constantValues = true,
          functionTypeParameters = true,
          parameterNames = true,
          rangeVariableTypes = true,
        },
        buildFlags = { "-tags", "integration" },
      },
    },
  },
  html = {},
  vue_ls = {
    -- Vue LSP (Volar/Vue Language Server)
    filetypes = { "typescript", "javascript", "javascriptreact", "typescriptreact", "vue", "json" },
    init_options = {
      vue = {
        hybridMode = false,
      },
    },
  },
  jsonls = {
    json = {
      schemas = require("schemastore").json.schemas(),
      validate = { enable = true },
    },
  },
  lua_ls = {
    Lua = {
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
    },
  },
  powershell_es = {},
  pylsp = {},
  omnisharp = {
    cmd = { "omnisharp", "--languageserver", "--hostPID", tostring(vim.fn.getpid()) },
    enable_roslyn_analyzers = true,
    organize_imports_on_format = true,
    enable_import_completion = true,
    enable_ms_build_load_projects_on_demand = false,
    settings = {
      FormattingOptions = {
        EnableEditorConfigSupport = true,
        OrganizeImports = true,
      },
      RoslynExtensionsOptions = {
        EnableAnalyzersSupport = true,
        EnableImportCompletion = true,
        EnableDecompilationSupport = true,
        AnalyzeOpenDocumentsOnly = false,
        InlayHintsOptions = {
          EnableForParameters = true,
          ForLiteralParameters = true,
          ForIndexerParameters = true,
          ForObjectCreationParameters = true,
          ForOtherParameters = true,
          SuppressForParametersThatDifferOnlyBySuffix = false,
          SuppressForParametersThatMatchMethodIntent = false,
          SuppressForParametersThatMatchArgumentName = false,
          EnableForTypes = true,
          ForImplicitVariableTypes = true,
          ForLambdaParameterTypes = true,
          ForImplicitObjectCreation = true,
        },
      },
      MsBuild = {
        LoadProjectsOnDemand = false,
      },
    },
  },
  rust_analyzer = {
    -- Note: rustaceanvim handles rust-analyzer configuration
    -- This entry ensures Mason installs rust-analyzer
    cmd = { "rust-analyzer" },
    filetypes = { "rust" },
    root_dir = require("lspconfig.util").root_pattern("Cargo.toml", "rust-project.json"),
    settings = {
      ["rust-analyzer"] = {
        cargo = {
          allFeatures = true,
          loadOutDirsFromCheck = true,
          runBuildScripts = true,
        },
        -- Add clippy lints for Rust
        checkOnSave = {
          allFeatures = true,
          command = "clippy",
          extraArgs = { "--no-deps" },
        },
        procMacro = {
          enable = true,
          ignored = {
            ["async-trait"] = { "async_trait" },
            ["napi-derive"] = { "napi" },
            ["async-recursion"] = { "async_recursion" },
          },
        },
      },
    },
  },
  terraformls = {},
  ts_ls = {},
  yamlls = {
    settings = {
      yaml = {
        format = {
          enable = true,
        },
        validate = true,
        schemaDownload = {
          enable = false,
        },
        schemas = {},
      },
      redhat = {
        telemetry = {
          enabled = false,
        },
      },
    },
  },
  -- roslyn is handled by roslyn.nvim plugin, not lspconfig
  zls = {},
  kotlin_language_server = {
    settings = {
      kotlin = {
        compiler = {
          jvm = {
            target = "17" -- Android typically uses Java 11 or 17
          }
        },
        completion = {
          snippets = {
            enabled = true
          }
        },
        linting = {
          debounceTime = 250
        },
        formatting = {
          formatter = "ktlint"
        },
        indexing = {
          enabled = true
        },
        externalSources = {
          useKlsScheme = true,
          autoConvertToKotlin = true
        }
      }
    },
    root_dir = require("lspconfig.util").root_pattern("settings.gradle", "settings.gradle.kts", "build.gradle",
      "build.gradle.kts", ".git"),
    single_file_support = true,
    init_options = {
      storagePath = vim.fn.stdpath("data") .. "/kotlin"
    }
  },
  jdtls = {
    -- jdtls is handled separately in ftplugin/java.lua for better control
    -- This is here to ensure Mason installs it
  },
  sourcekit = {
    cmd = {
      "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"
    },
    filetypes = { "swift", "objc", "objcpp" },
    root_dir = require("lspconfig.util").root_pattern("Package.swift", ".git", "project.yml", ".xcodeproj",
      ".xcworkspace"),
    settings = {
      sourcekit = {
        serverArguments = {
          "-Xswiftc", "-sdk",
          "-Xswiftc",
          "/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator.sdk",
          "-Xswiftc", "-target",
          "-Xswiftc", "arm64-apple-ios-simulator",
        }
      }
    },
    capabilities = {
      workspace = {
        didChangeWatchedFiles = {
          dynamicRegistration = true,
        },
      },
    },
  },
}

-- Setup neovim lua configuration
require("neodev").setup()

-- Setup mason so it can manage external tooling
require("mason").setup()

-- Ensure the servers above are installed
local mason_lspconfig = require("mason-lspconfig")

mason_lspconfig.setup({
  ensure_installed = vim.tbl_filter(function(name)
    -- Exclude sourcekit as it's provided by Xcode, not Mason
    return name ~= "sourcekit"
  end, vim.tbl_keys(servers)),
  automatic_enable = true,
})

for server, config in pairs(servers) do
  -- Skip rust-analyzer as it's handled by rustaceanvim
  if server == "rust_analyzer" then
    goto continue
  end

  config.on_attach = on_attach
  config.capabilities = capabilities

  -- This is a workaround for the omnisharp-roslyn issue with semantic tokens
  -- if server == "omnisharp" then
  --   config.capabilities.textDocument.semanticTokens.dynamicRegistration = false
  --   config.capabilities.textDocument.semanticTokens.tokenModifiers = {
  --     "static",
  --     "readonly",
  --     "deprecated",
  --     "abstract",
  --     "async",
  --     "modification",
  --   }
  --   config.capabilities.textDocument.semanticTokens.tokenTypes = {
  --     "comment",
  --     "keyword",
  --     "string",
  --     "number",
  --     "regexp",
  --     "operator",
  --     "namespace",
  --     "type",
  --     "struct",
  --     "class",
  --     "interface",
  --     "enum",
  --     "typeParameter",
  --     "function",
  --     "method",
  --     "macro",
  --     "property",
  --   }
  -- end

  require("lspconfig")[server].setup(config)

  ::continue::
end

-- Turn on lsp status information
require("fidget").setup()

-- nvim-cmp setup
local cmp = require("cmp")
local luasnip = require("luasnip")

cmp.setup({
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert({
    ["<C-d>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<CR>"] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    }),
    ["<Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { "i", "s" }),
    ["<S-Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { "i", "s" }),
  }),
  sources = {
    { name = "path" },
    { name = "nvim_lsp", keyword_length = 1 },
    { name = "nvim_lua", keyword_length = 1 },
    { name = "luasnip" }
  },
})
