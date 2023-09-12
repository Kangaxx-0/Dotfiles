local plugins = {
  {
    "williamboman/mason.nvim",
    opts = {
      ensure_installed = {
        "rust-analyzer",
        "typescript-language-server",
      },
    },
  },
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "jose-elias-alvarez/null-ls.nvim",
      config = function()
        require "custom.configs.null-ls"
      end,
    },

    config = function()
      require "plugins.configs.lspconfig"
      require "custom.configs.lspconfig"
    end,
  },
  {
    "jose-elias-alvarez/null-ls.nvim",
    event = "VeryLazy",
    opts = function()
      return require "custom.configs.null-ls"
    end,
  },
  {
    "f-person/git-blame.nvim",
    lazy = false,
  },
  {
    "folke/trouble.nvim",
    lazy = false,
  },
  {
    "github/copilot.vim",
    lazy = false,
  },
  {
    "simrat39/symbols-outline.nvim",
    lazy = false,
    require("symbols-outline").setup(),
  },
  {
    "lewis6991/spellsitter.nvim",
    lazy = false,
  },
  {
    "phaazon/hop.nvim",

    lazy = false,
    require("hop").setup {
      keys = "etovxqpdygfblzhckisuran",
    },
  },
}

return plugins
