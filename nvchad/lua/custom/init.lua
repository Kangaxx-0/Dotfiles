local opt = vim.opt

opt.swapfile = false
opt.spell = true
opt.spelllang = "en_us"
opt.number = true
opt.relativenumber = true
opt.jumpoptions = "stack"
opt.spellfile = "~/.config/nvim/spell/en.utf-8.add"

vim.cmd "behave mswin" -- <C-x><C-s> show popup menu to fix spell error
vim.cmd "set whichwrap+=<,>,[,],h,l"
vim.cmd [[set iskeyword+=-]]
vim.cmd [[set formatoptions-=cro]] -- TODO: this doesn't seem to work

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    virtual_text = false,
  }
)

-- Setup wsl Clipboard
local has = function(x)
  return vim.fn.has(x) == 1
end

if has("win32") then
  vim.opt.clipboard:prepend({ "unnamed", "unnamedplus" })
end

if has("wsl") then
  local clip = "/mnt/c/Windows/System32/clip.exe"
  --[[ local pwsh = "\"/mnt/c/Program Files/PowerShell/7/pwsh.exe\"" ]]
  --[[ local paste = pwsh .. ' -c [Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace("`r", ""))' ]]
  -- code of pwsh_clip.sh, and make sure powershell 7 is installed
  --   #!/bin/bash
  --   "/mnt/c/Program Files/PowerShell/7/pwsh.exe" -c "[Console]::Out.Write($(Get-Clipboard -Raw).tostring().replace('`r', ''))"
  --
  local paste = "/home/gaxx/pwsh_clip.sh"
  vim.g.clipboard = {
    name = "WslClipboard",
    copy = {
      ["+"] = clip,
      ["*"] = clip,
    },
    paste = {
      ["+"] = paste,
      ["*"] = paste,
    },
    cache_enabled = 0,
  }
elseif has("linux") then
  -- hogehoge
end

if has("mac") then
  vim.opt.clipboard:append({ "unnamedplus" })
end

