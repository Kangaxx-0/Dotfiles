local on_attach = require("plugins.configs.lspconfig").on_attach
local capabilities = require("plugins.configs.lspconfig").capabilities

local options = {
  server = {
    on_attach = on_attach,
    capabilities = capabilities,
  },
  {
    "mrcjkb/rustaceanvim",
    ft = { "rust" },
    depends = { "neovim/nvim-lspconfig" },
    opt = function()
      return require "custom.configs.rustaceanvim"
    end,
    config = function(_, opts)
      require("rustaceanvim").setup(opts)
    end,
  },
}
return options
