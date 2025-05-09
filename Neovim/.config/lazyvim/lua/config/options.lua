-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

-- I don't like the mouse moving the cursor in Vim while I'm switching
-- focus.
vim.opt.mouse = ""
vim.opt.background = "dark"

-- these work best after uienter
-- I'm setting the scheme in plugins/
-- vim.schedule(function()
--   vim.opt.background = "dark"
--   -- vim.cmd("colorscheme default")
--   vim.cmd("colorscheme mono-jade")
-- end)

-- Editorconfig doesn't do what I want well (completely) enough. While I do
-- have an .editorconig setup, here are my general defaults. For some specific
-- filetypes I also have settings in autocommands.
vim.opt.breakindent = true
vim.opt.autoindent = true
vim.opt.expandtab = false
-- will syntax defaults and lsp set these properlY?
vim.opt.shiftwidth = 0
vim.opt.softtabstop = 0
vim.opt.tabstop = 8

-- The basic spell checker has been pretty good so far. I keep my dictionary in
-- a non-standard location.
-- use stdpath("config") find these directories
vim.opt.spellfile = vim.fn.expand("~/Notepad/thes-and-dict/en.utf-8.add")
vim.opt.spelllang = { "en_us" }
vim.opt.thesaurus = vim.fn.expand("~/Notepad/thes-and-dict/thesaurus.txt")

-- Other assorted options.
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.scrolloff = 3
vim.opt.wildmenu = true
vim.opt.syntax = "on"
vim.opt.inccommand = "split"

-- I find 'set list' too distracting in langauges where tab is the preferred
-- indent. I'm leaving the listchars as is but will turn off list by default.
-- It can be toggled on with ':set list'
vim.opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }

-- This should filter out noise in completion search. I was seeing spurious
-- "pattern not found" when completion was on for luals when I switched to
-- 0.11.?.
vim.opt.shortmess:append("c")
