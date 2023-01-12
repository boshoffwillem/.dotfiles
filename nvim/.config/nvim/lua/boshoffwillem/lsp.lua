vim.opt.signcolumn = 'yes' -- Reserve space for diagnostic icons

-- Mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist)

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(_, bufnr)
  local nmap = function(keys, func, desc)
    if desc then
      desc = 'LSP: ' .. desc
    end

    vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
  end

  nmap('gD', vim.lsp.buf.declaration, '[G]oto [D]eclaration')
  nmap('gd', vim.lsp.buf.definition, '')
  nmap('gr', vim.lsp.buf.references, '')
  nmap('gi', vim.lsp.buf.implementation, '')
  nmap('K', vim.lsp.buf.hover, '')
  nmap('<C-k>', vim.lsp.buf.signature_help, '')
  nmap('<space>wa', vim.lsp.buf.add_workspace_folder, '')
  nmap('<space>wr', vim.lsp.buf.remove_workspace_folder, '')
  nmap('<space>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, '')
  nmap('<space>D', vim.lsp.buf.type_definition, '')
  nmap('<space>lr', vim.lsp.buf.rename, '')
  nmap('<space>la', vim.lsp.buf.code_action, '')
  nmap('<space>l=', function() vim.lsp.buf.format { async = true } end, '')
  nmap('<leader>ds', require('telescope.builtin').lsp_document_symbols, '')
  nmap('<leader>ws', require('telescope.builtin').lsp_dynamic_workspace_symbols, '')

  -- Create a command `:Format` local to the LSP buffer
  vim.api.nvim_buf_create_user_command(bufnr, 'Format', function(_)
    vim.lsp.buf.format()
  end, { desc = 'Format current buffer with LSP' })
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)
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
  jsonls = {
    json = {
      schemas = require('schemastore').json.schemas(),
      validate = { enable = true },
    },
  },
  rust_analyzer = {},
  sumneko_lua = {
    Lua = {
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
    },
  },
  yamlls = {
    yaml = {
      format = true,
      schemaDownload = {
        enable = true
      },
      schemas = {
        ["https://raw.githubusercontent.com/microsoft/azure-pipelines-vscode/master/service-schema.json"] = { "/*Pipeline*.yml" },
        ["https://raw.githubusercontent.com/instrumenta/kubernetes-json-schema/master/v1.18.0-standalone-strict/all.json"] = "/*.k8s.yaml"
      },
      validate = true
    }
  }
}

-- Setup neovim lua configuration
require('neodev').setup()

-- Setup mason so it can manage external tooling
require('mason').setup()

-- Ensure the servers above are installed
local mason_lspconfig = require 'mason-lspconfig'

mason_lspconfig.setup {
  ensure_installed = vim.tbl_keys(servers),
}

mason_lspconfig.setup_handlers {
  function(server_name)
    require('lspconfig')[server_name].setup {
      capabilities = capabilities,
      on_attach = on_attach,
      flags = lsp_flags,
      settings = servers[server_name],
    }
  end,
}

-- local pid = vim.fn.getpid()
-- mason_lspconfig.setup_handlers {
--   function(server_name)
--     require('lspconfig')['omnisharp'].setup {
--       capabilities = capabilities,
--       on_attach = on_attach,
--       flags = lsp_flags,
--       settings = servers[server_name],
--       cmd = { "OmniSharp", "-lsp", "--hostPID", tostring(pid) },
--
--       -- Enables support for reading code style, naming convention and analyzer
--       -- settings from .editorconfig.
--       enable_editorconfig_support = true,
--
--       -- If true, MSBuild project system will only load projects for files that
--       -- were opened in the editor. This setting is useful for big C# codebases
--       -- and allows for faster initialization of code navigation features only
--       -- for projects that are relevant to code that is being edited. With this
--       -- setting enabled OmniSharp may load fewer projects and may thus display
--       -- incomplete reference lists for symbols.
--       enable_ms_build_load_projects_on_demand = false,
--
--       -- Enables support for roslyn analyzers, code fixes and rulesets.
--       enable_roslyn_analyzers = false,
--
--       -- Specifies whether 'using' directives should be grouped and sorted during
--       -- document formatting.
--       organize_imports_on_format = false,
--
--       -- Enables support for showing unimported types and unimported extension
--       -- methods in completion lists. When committed, the appropriate using
--       -- directive will be added at the top of the current file. This option can
--       -- have a negative impact on initial completion responsiveness,
--       -- particularly for the first few completion sessions after opening a
--       -- solution.
--       enable_import_completion = false,
--
--       -- Specifies whether to include preview versions of the .NET SDK when
--       -- determining which version to use for project loading.
--       sdk_include_prereleases = true,
--
--       -- Only run analyzers against open files when 'enableRoslynAnalyzers' is
--       -- true
--       analyze_open_documents_only = false,
--
--       init_options = {},
--
--       filetypes = { "cs", "vb" },
--
--       -- root_dir = root_pattern(".sln") or root_pattern(".csproj")
--     }
--   end,
-- }

-- Turn on lsp status information
require('fidget').setup()

-- nvim-cmp setup
local cmp = require 'cmp'
local luasnip = require 'luasnip'

cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = cmp.mapping.preset.insert {
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end, { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end, { 'i', 's' }),
  },
  sources = {
    { name = 'buffer' },
    { name = 'path' },
    { name = 'nvim_lsp', keyword_length = 1 },
    { name = 'nvim_lua', keyword_length = 1 },
    { name = 'cmp_tabnine' },
  },
}
