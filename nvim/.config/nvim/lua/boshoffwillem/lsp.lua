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
local lsp_flags = {
  -- This is the default in Nvim 0.7+
  debounce_text_changes = 150,
}

-- Enable the following language servers
--  Feel free to add/remove any LSPs that you want here. They will automatically be installed.
--
--  Add any additional override configuration in the following tables. They will be passed to
--  the `settings` field of the server config. You must look up that documentation yourself.

local servers = {
  angularls = {},
  azure_pipelines_ls = {
    yaml = {
      schemaStore = {
        -- You must disable built-in schemaStore support if you want to use
        -- this plugin and its advanced options like `ignore`.
        enable = false,
        -- Avoid TypeError: Cannot read properties of undefined (reading 'length')
        url = "",
      },
      schemas = require("schemastore").yaml.schemas(),
    },
  },
  dartls = {
    cmd = { "/Users/boshoffwillem/development/flutter/bin/dart", "language-server", "--protocol=lsp" },
    filetypes = { "dart" },
    init_options = {
      onlyAnalyzeProjectsWithOpenFiles = false,
      suggestFromUnimportedLibraries = true,
      closingLabels = true,
      outline = true,
      flutterOutline = true,
    },
    settings = {
      dart = {
        completeFunctionCalls = true,
        showTodos = true,
      }
    },
  },
  -- fsautocomplete = {},
  dockerls = {},
  docker_compose_language_service = {},
  html = {},
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
    cmd = { "omnisharp" },
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
  rust_analyzer = {},
  terraformls = {},
  ts_ls = {},
  yamlls = {
    yaml = {
      format = true,
      schemaDownload = {
        enable = true,
      },
      schemas = {
        ["https://raw.githubusercontent.com/instrumenta/kubernetes-json-schema/master/v1.18.0-standalone-strict/all.json"] = "/*.k8s.yaml",
      },
      validate = true,
    },
  },
  roslyn = {
    config = {
      on_attach = function(client, bufnr)
        client.server_capabilities.semanticTokensProvider = nil
        on_attach(client, bufnr)
      end,
    }
  },
  zls = {},
}

-- Setup neovim lua configuration
require("neodev").setup()

-- Setup mason so it can manage external tooling
require("mason").setup()

-- Ensure the servers above are installed
local mason_lspconfig = require("mason-lspconfig")

mason_lspconfig.setup({
  ensure_installed = vim.tbl_filter(function(name)
    return name ~= "dartls"  -- dartls is managed by flutter-tools, not mason
  end, vim.tbl_keys(servers)),
  automatic_enable = true,
})

for server, config in pairs(servers) do
  if server ~= "dartls" then  -- dartls is handled by flutter-tools
    config.on_attach = on_attach
    config.capabilities = capabilities
    config.flags = lsp_flags

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
  end
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
    { name = "supermaven" },
  },
})
