require("kickstart.defaults")
require("kickstart.lazy")

require("neotest").setup({
  adapters = {
    require("neotest-dotnet"),
  },
})

require("dap-scope-walker").setup({
  interval = 250,
})
