-- Autocmds are automatically loaded on the VeryLazy event
-- Default autocmds that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/autocmds.lua
--
-- Add any additional autocmds here
-- with `vim.api.nvim_create_autocmd`
--
-- Or remove existing autocmds by their group name (which is prefixed with `lazyvim_` for the defaults)
-- e.g. vim.api.nvim_del_augroup_by_name("lazyvim_wrap_spell")

-- This is how I like plaintext formatted: I set ai instead of noai so that
-- hanging indents work.

local textgroup = vim.api.nvim_create_augroup("text", { clear = true })
vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
  pattern = { "*.txt", "*.me", "*.md", "README*", "LICENSE*" },
  group = textgroup,
  callback = function()
    vim.cmd([[
      set ai et ts=5 sw=5 tw=70 fo-=t fo+=2n
      ]])
  end,
})

local markdowngroup = vim.api.nvim_create_augroup("markdown", { clear = true })
vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
  pattern = { "*.md", "*.markdown", "README*", "LICENSE*" },
  group = markdowngroup,
  callback = function()
    vim.cmd([[
      highlight ExtraWhitespace ctermbg=white guibg=gray
      match ExtraWhitespace /\s\{2,\}$/
      set spell
      set tw=70
   ]])
  end,
})

local codegroup = vim.api.nvim_create_augroup("code", { clear = true })
vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
  pattern = { "*.c", "*.f90", "*.odin" },
  group = codegroup,
  callback = function()
    vim.cmd([[
      set fo+=c fo+=r fo+=/ fo+=o fo+=q
      ]])
  end,
})

local commentgroup = vim.api.nvim_create_augroup("comment", { clear = true })
vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
  pattern = { "*.odin" },
  group = commentgroup,
  callback = function()
    vim.cmd([[ set commentstring=/*\ %s\ */ ]])
  end,
})
