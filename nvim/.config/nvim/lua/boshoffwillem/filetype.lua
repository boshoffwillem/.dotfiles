local status_ok, filetype = pcall(require, "filetype")
if not status_ok then
  return
end

-- In init.lua or filetype.nvim's config file
filetype.setup {
  enabled = true,
  overrides = {
    complex = {
      ["Directory.Build.props"] = "xml",
      ["nuget.config"] = "xml",
    },
  },
}
