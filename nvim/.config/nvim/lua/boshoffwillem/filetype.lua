local status_ok, filetype = pcall(require, "filetype")
if not status_ok then
  return
end

-- In init.lua or filetype.nvim's config file
filetype.setup({
  enabled = true,
  extensions = {
    overrides = {
      razor = "html",
    },
    complex = {
      ["Directory.Build.props"] = "xml",
      ["nuget.config"] = "xml",
      ["azure-pipelines.yml"] = "yaml.azurepipelines",
      ["azure-pipelines.yaml"] = "yaml.azurepipelines",
    },
    literal = {
      [".azure-pipelines"] = function(path, bufnr)
        if path:match("%.ya?ml$") then
          return "yaml.azure"
        end
      end,
    },
    pattern = {
      ["azure%-pipelines.*%.ya?ml"] = "yaml.azurepipelines",
    },
  },
})
