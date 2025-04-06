-- init.lua -- an init config for neovim 0.11 -- April 2025

-- Using folke/lazy.nvim and trying to keep the config small.

-- [[ First Thing ]]

-- This can actually be done anytime before your first plugin loads, but 'twer
-- best done quickly!

vim.g.mapleader = " "
vim.g.maplocalleader = ";"

-- [[ UI Appearance ]]

-- Fonts and trim. But note that you can't change the colorscheme to one that
-- you load via Lazy (and probably some other package managers) until after
-- they have done their thing). For Lazy that's a call to setup much later in
-- this init. I want my settings established before loading any plugins.

vim.opt.termguicolors = true
vim.opt.background = "dark"
vim.g.have_nerd_font = true
vim.opt.number = true
vim.opt.relativenumber = false
vim.opt.signcolumn = "yes"
vim.opt.showmode = false -- status line should do this --
vim.opt.winborder = "rounded" -- others double, single, solid, shadow, none
vim.opt.ruler = true
vim.opt.title = true
vim.opt.cursorline = true

-- I find 'set list' too distracting in langauges where tab is the preferred
-- indent. I'm leaving the listchars as is but will turn off list by default.
-- It can be toggled on with ':set list'

vim.opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }

-- [[ UI behavior ]]

-- Here are the things in the UI that are usually triggered by or are related
-- to a user activity.

-- This should filter out noise in completion search. I was seeing spurious
-- "pattern not found" when completion was on for luals.

vim.opt.shortmess:append("c")

-- Preview substitutions in a split as the replace is composed.

vim.opt.inccommand = "split"

-- I'm struggling with learning the Neovim completion keys. Options I've seen
-- that some people find helpful are fuzzy, noinsert, and noselect.

vim.cmd("set completeopt+=fuzzy")
vim.cmd("set completeopt+=noinsert")

-- These are obvious settings that need no explanation.

vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.scrolloff = 3
vim.opt.wildmenu = true
vim.opt.showcmd = true
vim.opt.showmatch = true
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.syntax = "on"

-- [[ undo, clipboard, mouse, keyboard ]]

-- We'll leave the mouse on for now, but I might turn it off later.

vim.opt.mouse = "a" -- maybe not?

-- Sync clipboard between OS and Neovim.

vim.opt.undofile = true

--  Schedule this setting after `UiEnter` because it can increase startup-time.

vim.schedule(function()
   vim.opt.clipboard = "unnamedplus"
end)

-- Shorten some keyboard checks.

vim.opt.updatetime = 250
vim.opt.timeoutlen = 300

-- [[ Line breaks and text formatting ]]

-- Editorconfig doesn't do what I want well (completely) enough. While I do
-- have an .editorconig setup, here are my general defaults. For some specific
-- filetypes I also have settings in autocommands.

vim.opt.breakindent = true
vim.opt.autoindent = true
vim.opt.expandtab = true
vim.opt.shiftwidth = 3
vim.opt.tabstop = 8

-- [[ Basic text editing and formatting ]]

-- The basic spell checker has been prettygood so far. I keep my dictionary in
-- a non-standard location.

vim.opt.spelllang = { "en_us" }
vim.opt.spellfile = vim.fn.expand("~/.local/nvim_spell/en.utf-8.add")

-- This is how I like plaintext formatted: I set ai instead of noai so that
-- hanging indents work.

local textgroup = vim.api.nvim_create_augroup("text", { clear = true })
vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
   pattern = { "*.txt", "*.me", "*.md", "README*", "LICENSE*" },
   group = textgroup,
   callback = function()
      vim.cmd([[
      highlight ExtraWhitespace ctermbg=white guibg=gray
      match ExtraWhitespace /\s\+$/
      set ai et ts=5 sw=5 tw=70 fo-=t fo+=2n
      ]])
   end,
})

-- [[ Common Keymappings  ]]

-- These can get messy and I try to use defaults when I can stand them. But
-- some mappings are unavoidable and best done here.

-- Turn off search highlights.

vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")

-- Open quickfix.

vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Open [Q]uickfix list" })

-- Exit terminal mode in the builtin terminal with an easier to remember
-- shortcut.

vim.keymap.set("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })

-- I turn off arrow keys in normal mode to avoid mouse/touchpad scrolling. They
-- are still on for insert and visual mode, but I may someday be able to turn
-- them and the mouse completely off in Neovim.

vim.keymap.set("n", "<left>", "")
vim.keymap.set("n", "<right>", "")
vim.keymap.set("n", "<up>", "")
vim.keymap.set("n", "<down>", "")

-- Use CTRL+<hjkl> to switch between windows.

vim.keymap.set("n", "<C-h>", "<C-w><C-h>", { desc = "Move focus to the left window" })
vim.keymap.set("n", "<C-l>", "<C-w><C-l>", { desc = "Move focus to the right window" })
vim.keymap.set("n", "<C-j>", "<C-w><C-j>", { desc = "Move focus to the lower window" })
vim.keymap.set("n", "<C-k>", "<C-w><C-k>", { desc = "Move focus to the upper window" })

-- Highlight yanked text.

vim.api.nvim_create_autocmd("TextYankPost", {
   desc = "Highlight when yanking (copying) text",
   group = vim.api.nvim_create_augroup("highlight-yank", { clear = true }),
   callback = function()
      vim.hl.on_yank()
   end,
})

-- [[ Start plugin manager ]]

-- I'm using 'folke/lazy.nvim' and find it nice. This first bit of code will
-- download Lazy in a new Neovim configuration.
--
-- You MUST set vim.g.mapleader and maplocalleader before invoking Lazy
-- otherwise plugins that map based on leader keys will have broken mappings.

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
   local lazyrepo = "https://github.com/folke/lazy.nvim.git"
   local out =
      vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
   if vim.v.shell_error ~= 0 then
      vim.api.nvim_echo({
         { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
         { out, "WarningMsg" },
         { "\nPress any key to exit..." },
      }, true, {})
      vim.fn.getchar()
      os.exit(1)
   end
end

-- Lazy exists, start it for this session. Plugins are downloaded to lazypath
-- and that is placed at the front of the runtime path. I keep my plugin
-- configurations under lua/plugins. Generally one file per plugin, named with
-- repo name (possibly minus trailing '.nvim').

vim.opt.rtp:prepend(lazypath)
require("lazy").setup({
   spec = { { import = "plugins" } },
   install = { colorscheme = { "default" } },
   checker = { enabled = false },
})

-- [[ Colorscheme and themes ]]

-- If the colorscheme is from a plugin, it won't be found until after Lazy
-- has initialized.

vim.opt.background = "dark"
vim.cmd("colorscheme pax")

-- [[ Lualine customization ]]

-- I tried to put these customizations in the plugin configuration file but
-- they didn't take. TODO: can I find a way to move them out of init.lua?

-- This attempts to replace lualine's diff totals with those already available
-- from gitsigns, hoping to avoid duplicate work. However as written it loses
-- some other information and eye candy din the lualine_b git setment. TODO:
-- can I fix this?
--
-- local function diff_source()
--    local gitsigns = vim.b.gitsigns_status_dict
--    if gitsigns then
--       return {
--          added = gitsigns.added,
--          modified = gitsigns.changed,
--          removed = gitsigns.removed,
--       }
--    end
-- end
--
-- To enable, add this to line to the sections table in lualine setup.
--
-- lualine_b = { { "diff", source = diff_source } },

-- Customize/override lualine sections here. I don't like the penguin
-- icon for *Nix style end of line so I've got old school.

require("lualine").setup({
   sections = {
      lualine_x = {
         {
            "fileformat",
            icons_enabled = true,
            symbols = {
               unix = "LF",
               dos = "CRLF",
               mac = "CR",
            },
         },
      },
   },
})

-- [[ LSP support ]]

-- I'm having some sort of coordination issue when I try to move some of this
-- down into the lsp directory. I know I should be able to make this more
-- modular but that's a future TODO: item.
--
-- Most of these configurations come from reading nvim-lspconfig, but as that
-- plugin shouldn't be needed these days I'm not loading it.

-- Lua Language Server --

vim.lsp.config["lua-language-server"] = {
   cmd = { "lua-language-server" },
   root_markers = { ".luarc.json", ".git", ".stylua" },
   settings = {
      Lua = {
         runtime = {
            version = "LuaJIT",
         },
      },
   },
   filetypes = { "lua" },
}
vim.lsp.enable("lua-language-server")

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

-- Prose, Marksman for Markdown first --

vim.lsp.config["marksman"] = {
   cmd = { "marksman", "server" },
   filetypes = { "markdown", "markdown.mdx" },
   root_markers = { ".git" },
}
vim.lsp.enable("marksman")

-- [[ Autocommands ]]

-- LspAttach:
--
-- Enable things that should be when LSP is attached. I prefer to format on
-- save using conform so formatexpr is set to nil. If indenting becomes an
-- issue, I'll need to handle that in Treesitter.

vim.api.nvim_create_autocmd("LspAttach", {
   callback = function(ev)
      local client = vim.lsp.get_client_by_id(ev.data.client_id)
      if client:supports_method("textDocument/completion") then
         vim.lsp.completion.enable(true, client.id, ev.buf, { autotrigger = "true" })
      end
      vim.bo[ev.buf].formatexpr = nil
   end,
})

-- [[ End Notes ]]
--
-- I had some autocommands here for language overrides but I've managed to move
-- most of them elsewhere, usually in after/ftplugin. I'm trying to keep the
-- init file as small as I reasonably can.
