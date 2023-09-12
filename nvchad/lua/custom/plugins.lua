local plugins = {
  -- default_plugin_config_replace = {
  --   nvterm = {
  --     type_opts = {
  --       float = {
  --         relative = "editor",
  --         row = 0.7,
  --         col = 0.7,
  --         width = 0.7,
  --         height = 0.7,
  --         border = "single",
  --       },
  --       behavior = {
  --         autoclose_on_quit = {
  --           enabled = true,
  --           confirm = false,
  --         },
  --         close_on_exit = true,
  --         auto_insert = true,
  --       },
  --     },
  --   },
  -- },
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
  {
    "akinsho/toggleterm.nvim",
    lazy = false,
    require("toggleterm").setup {
      size = 20,
      open_mapping = [[<c-\>]],
      hide_numbers = true,
      shade_filetypes = {},
      shade_terminals = true,
      shading_factor = 2,
      start_in_insert = true,
      insert_mappings = true,
      persist_size = true,
      direction = "float",
      close_on_exit = true,
      shell = vim.o.shell,
      float_opts = {
        border = "curved",
        winblend = 0,
        highlights = {
          border = "Normal",
          background = "Normal",
        },
      },
    },
  },
}

return plugins
