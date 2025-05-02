-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

-- these work best after uienter
vim.schedule(function()
  vim.opt.background = "dark"
  vim.cmd("colorscheme default")
  -- vim.cmd("colorscheme amber")
  -- vim.cmd("colorscheme mono-jade")
  --  vim.cmd("colorscheme amberchrome")
  -- vim.cmd("colorscheme malotru")
  -- vim.cmd("colorscheme lucius")
  -- vim.cmd("LuciusBlackHighContrast")
end)

-- [[ Line breaks and text formatting ]]

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

-- [[ Basic text editing and formatting ]]

-- The basic spell checker has been pretty good so far. I keep my dictionary in
-- a non-standard location.
-- use stdpath("config") find these directories
vim.opt.spellfile = vim.fn.expand("~/Notepad/thes-and-dict/en.utf-8.add")
vim.opt.spelllang = { "en_us" }
vim.opt.thesaurus = vim.fn.expand("~/Notepad/thes-and-dict/thesaurus.txt")

-- I'm struggling with learning the Neovim completion keys. Options I've seen
-- that some people find helpful are fuzzy, noinsert, and noselect.
-- I'm pulling these in as comments from my earlier config, I'll try to
-- do what lazyvim is set up for.
-- vim.cmd("set completeopt+=fuzzy")
-- vim.cmd("set completeopt+=noinsert")
-- vim.cmd("set completeopt+=noselect")

-- Other assorted options.
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.scrolloff = 3
vim.opt.wildmenu = true
vim.opt.syntax = "on"

-- This is kinda cool
vim.opt.inccommand = "split"

-- I find 'set list' too distracting in langauges where tab is the preferred
-- indent. I'm leaving the listchars as is but will turn off list by default.
-- It can be toggled on with ':set list'

vim.opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }

-- This should filter out noise in completion search. I was seeing spurious
-- "pattern not found" when completion was on for luals.

vim.opt.shortmess:append("c")

-- NOTE: Is there a better place for this?

-- Odin Language Server --

vim.lsp.config["ols"] = {
  cmd = { "ols" },
  filetypes = { "odin" },
  root_markers = { ".git", "ols.json" },
}
vim.lsp.enable("ols")

-- Clangd --

vim.lsp.config["clangd"] = {
  cmd = {
    "clangd",
    "--background-index",
    "--pch-storage=memory",
    "--enable-config",
    "--log=info",
    "--header-insertion=never",
    "--completion-style=detailed",
    "--function-arg-placeholders",
  },
  filetypes = { "c", "cpp", "h", "hpp" },
  root_markers = { ".git", "CMakeLists.txt", "Makefile", "compile_commands.json" },
}
vim.lsp.enable("clangd")

-- I'm having problems with alacritty.toml so we'll try a few different things to
-- get past them.

vim.lsp.config["taplo"] = {
  filetypes = { 'toml' },
  -- IMPORTANT: this seems to handle not begin in git?
  root_markers = { "*.toml", "*.git" },
}
vim.lsp.enable("tapolo")
