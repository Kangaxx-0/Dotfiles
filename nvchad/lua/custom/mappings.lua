local M = {}

-- In order to disable a default keymap, use
M.disabled = {
  n = {
    ["<A-l>"] = "",
    ["<A-h>"] = "",
    ["<C-a>"] = "",
    ["<C-h>"] = "",
    ["<C-j>"] = "",
    ["<C-k>"] = "",
    ["<C-l>"] = "",
    ["<leader>gb"] = "",
    ["<leader>gc"] = "",
    ["<leader>gg"] = "",
    ["<leader>ba"] = "",
    ["<leader>pp"] = "",
    ["<leader>sp"] = "",
    ["<leader>fm"] = "",
    ["<leader>ra"] = "",
    ["<leader>rn"] = "",
    ["<leader>f"] = "",
    ["<leader>wK"] = "",
    ["<leader>pt"] = "",
    ["<leader><leader>"] = "",
    ["<leader>q"] = "",
    ["<leader>D"] = "",
    ["<leader>e"] = "",
    ["<leader>n"] = "",
    ["<leader>v"] = "",
    ["<leader>x"] = "",
    ["<A-1>"] = "",
    ["<A-f>"] = "",
    ["<C-Up>"] = "",
    ["<C-Down>"] = "",
    ["<C-Left>"] = "",
    ["<C-Right>"] = "",
    ["<C-[>"] = "",
    ["<C-]>"] = "",
  },
  v = {
    ["<A-Up>"] = "",
    ["<A-Down>"] = "",
  },
  x = {
    ["J"] = "",
    ["K"] = "",
    ["<A-j>"] = "",
    ["<A-k>"] = "",
  },
}

-- Your custom mappings
M.gaxx = {
  n = {
    -- Move tab/buffers
    ["<A-l>"] = {
      function()
        require("nvchad.tabufline").tabuflineNext()
      end,
      "Move to next buffer",
    },
    ["<A-h>"] = {
      function()
        require("nvchad.tabufline").tabuflinePrev()
      end,
      "Move to previous buffer",
    },
    ["<leader>ba"] = { "<cmd>bufdo bd <CR>", "Close all buffers" },
    ["<leader>bo"] = {
      function()
        local current_buf = vim.api.nvim_get_current_buf()
        for _, buf in ipairs(vim.api.nvim_list_bufs()) do
          if buf ~= current_buf then
            vim.api.nvim_buf_delete(buf, {})
          end
        end
      end,
      "Close others",
    },
    ["<leader>wc"] = { "<cmd>quit <CR>", "Close window" },
    ["<leader>q"] = {
      function()
        local current_buf = vim.fn.bufnr "%"
        vim.cmd "silent! bprev"
        -- If the buffer numbers are the same, go to the next buffer
        if vim.fn.bufnr "%" == current_buf then
          vim.cmd "silent! bnext"
        end
        -- Delete the original buffer
        vim.cmd("bdelete " .. current_buf)
      end,
      "Close buffer",
    },
    -- Manage windows
    ["<leader>wh"] = { "<C-w>h", "Move left window" },
    ["<leader>wj"] = { "<C-w>j", "Move down window" },
    ["<leader>wk"] = { "<C-w>k", "Move up window" },
    ["<leader>wl"] = { "<C-w>l", "Move right window" },
    ["<leader>wv"] = { "<cmd>vsplit <CR>", "Vertical split" },
    ["<leader>ws"] = { "<cmd>split <CR>", "Horizontal split" },
    ["gl"] = {
      function()
        vim.diagnostic.open_float { border = "rounded" }
      end,
      "Floating diagnostic",
    },
    ["<C-Up>"] = { ":resize +2 <CR>", "Increase window height" },
    ["<C-Down>"] = { ":resize -2 <CR>", "Decrease window height" },
    ["<C-Left>"] = { ":vertical resize +2 <CR>", "Increase window width" },
    ["<C-Right>"] = { ":vertical resize -2 <CR>", "Decrease window width" },
    -- Git
    ["<leader>gg"] = {
      function()
        require "custom.toggleterm"
        _LAZYGIT_TOGGLE()
      end,
      "lazygit",
    },
    ["<leader>gb"] = { "<cmd>GitBlameToggle <CR>", "Toggle git blame" },
    ["<leader>gc"] = { "<cmd>GitBlameCopyCommitURL <CR>", "Copy commit url" },
    -- Editing
    ["<C-a>"] = { "Select all", "Select all text in buffer" },
    ["<leader>cr"] = {
      function()
        require("nvchad.renamer").open()
      end,
      "LSP rename",
    },
    ["<leader>cf"] = {
      function()
        vim.lsp.buf.format { async = true }
      end,
      "LSP formatting",
    },
    ["f"] = {
      function()
        require("hop").hint_char1 { direction = require("hop.hint").HintDirection.AFTER_CURSOR, current_line_only = true }
      end,
      "Hop forward",
    },
    ["F"] = {
      function()
        require("hop").hint_char1 {
          direction = require("hop.hint").HintDirection.BEFORE_CURSOR,
          current_line_only = true,
        }
      end,
      "Hop Backward",
    },
    ["s"] = {
      function()
        require("hop").hint_char2 { direction = require("hop.hint").HintDirection.AFTER_CURSOR, current_line_only = true }
      end,
      "Hop forward x 2",
    },
    ["S"] = {
      function()
        require("hop").hint_char2 {
          direction = require("hop.hint").HintDirection.BEFORE_CURSOR,
          current_line_only = true,
        }
      end,
      "Hop Backward x 2",
    },
    ["<leader>cx"] = { "<cmd>Trouble <CR>", "List All" },
    ["<leader>cc"] = { "<cmd>Copilot panel <CR>", "Copilot Suggestion" },
    ["<A-1>"] = { "<cmd>NvimTreeToggle <CR>", "Toggle file explorer" },
    ["<A-f>"] = { "<cmd>SymbolsOutline <CR>", "Toggle symbols outline" },
    -- Other
    ["<leader>tc"] = { "<cmd>e ~/.config/nvim/lua/<cr>", "Find private config" },
  },

  i = {
    ["jk"] = { "<ESC>", "escape insert mode", opts = { nowait = true } },
  },

  v = {
    [">"] = { ">gc", "Identation -> Right" },
    ["<"] = { "<gc", "Identation -> Left" },
    ["<A-j>"] = { ":m .+1<CR>==", "Move text up" },
    ["<A-k>"] = { ":m .-2<CR>==", "Move text down" },
    -- keymap("v", "p", '"_dP', opts)
  },

  x = {
    ["J"] = { ":move '>+1<CR>gv-gv", "Move line down" },
    ["K"] = { ":move '<-2<CR>gv-gv", "Move line up" },
    ["<A-j>"] = { ":move '>+1<CR>gv-gv", "Move line down" },
    ["<A-k>"] = { ":move '<-2<CR>gv-gv", "Move line up" },
  },
}

return M
