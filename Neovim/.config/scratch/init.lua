-- ~/.config/mininvim/lua/config/options.lua

--- Remove the @ from @diagnostic below to see all the undefined
--- globals that haven't been swatted yet.
---@diagnostic disable:undefined-global

-- Set various options and defaults that don't require any plugins. I'm
-- going to use lazy for package management this trial, but avoid as much
-- of the snacks and noice as possible. One package manager should be
-- sufficient for me to get a basic system going.

--- Boostrap Lazy.nvim -----------------------------------------------

-- This is the stock boostrap code.

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
   local lazyrepo = "https://github.com/folke/lazy.nvim.git"
   local out = vim.fn.system({
      "git",
      "clone",
      "--filter=blob:none",
      "--branch=stable",
      lazyrepo,
      lazypath,
   })
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
vim.opt.rtp:prepend(lazypath)

--- Set a few things before any plugin code fires --------------------

-- I think these should be set before doing much of anything else. Having
-- `mapleader` and `maplocalleader` set before loading plugins avoids broken
-- plugin keymaps. Space sometimes causes issues when used for leader but
-- in my experience remaping it to nop clears them up. I'll set other
-- options after plugins are loaded.

local g = vim.g
local o = vim.opt

vim.cmd("nnoremap <space> <nop>")
g.mapleader = " "
g.maplocalleader = "\\"
o.mouse = "" -- mouse clicks shouldn't move the cursor
o.termguicolors = true
o.background = "dark"
g.have_nerd_font = true
vim.schedule(function()
   -- Schedule this setting after `UiEnter` because it can increase startup-time.
   vim.opt.clipboard = "unnamedplus"
end)
o.updatetime = 250 -- Shorten some keyboard checks.
o.timeoutlen = 300

--- Plugin specifications for lazy.nvim ------------------------------

-- Folke's examples load plugins first thing. They could be imported but
-- I'm going single file for now.

require("lazy").setup({
   spec = {

      --- First, let's get the color scheme set -----------------------

      {
         "projekt0n/github-nvim-theme",
         lazy = false,
         priority = 1000,
         config = function() vim.cmd("colorscheme github_dark_high_contrast") end,
      },

      { "nvim-tree/nvim-web-devicons", lazy = true },

      {
         "nvim-lualine/lualine.nvim",
         dependencies = { "nvim-tree/nvim-web-devicons" },
         opts = {
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
         },
      },

      --- Several standard zero-configuration plugins -----------------

      { "folke/which-key.nvim", lazy = true },

      --- Treesitter has some post install requirements ---------------

      -- Most of this is copied out of lazyvim's definition.
      -- See lazyvim/lua/lazyvim/plugins/treesitter.lua.

      {
         "nvim-treesitter/nvim-treesitter",
         build = ":TSUpdate",
         event = { "VeryLazy" },
         lazy = vim.fn.argc(-1) == 0,
         init = function(plugin)
            require("lazy.core.loader").add_to_rtp(plugin)
            require("nvim-treesitter.query_predicates")
         end,
         cmd = { "TSUpdateSync", "TSUpdate", "TSInstall" },
         opts = {
            sync_install = false,
            auto_installl = true,
            highlight = {
               enable = true,
               additional_vim_regex_highlighting = { "ruby" },
            },
            ensure_installed = {
               "bash",
               "c",
               "diff",
               "fortran",
               "html",
               "json",
               "lua",
               "markdown",
               "odin",
               "python",
               "regex",
               "ruby",
               "toml",
               "typescript",
               "vim",
               "vimdoc",
               "xml",
               "yaml",
            },
            -- TODO: what about incremental_selection and textobjects?
            incremental_selection = { enable = true },
            indent = { enable = true, disable = { "ruby" } },
         },
         config = function(_, opts)
            require("nvim-treesitter.configs").setup(opts)
         end,
      },

      -- This is paired with nvim-treesitter, see comments there.

      { "nvim-treesitter/nvim-treesitter-textobjects", event = "VeryLazy" },

      --- Git diffs and hunks and stats -------------------------------

      { "lewis6991/gitsigns.nvim", event = "VeryLazy" },

      --- Mason installs binary dependencies for lsp and ts -----------

      {
         "mason-org/mason.nvim",
         opts = {
            ui = {
               icons = {
                  package_installed = "✓",
                  package_pending = "➜",
                  package_uninstalled = "✗",
               },
            },
         },
      },

      --- Use conform.nvim for file formatting ------------------------

      -- The formatexpr being set to conform should prevent LSP from getting in the
      -- way if I have a formatter configured. It is expected that the conform
      -- formatexpr will fall back to the lsp formatexpr which should fall back to
      -- normal Vim formatting.

      {
         "stevearc/conform.nvim",
         event = { "BufWritePre" },
         cmd = { "ConformInfo" },
         opts = {
            formatters_by_ft = {
               bash = { "shfmt" },
               c = { "clang-format" },
               fortran = { "fprettify" },
               javascript = { "prettier" },
               json = { "prettier" },
               lua = {
                  "stylua",
                  opts = { args = "--search-parent-directories" },
               },
               markdown = { "prettier" },
               python = { "ruff" },
               ruby = { "rubocop" },
               toml = { "taplo" },
               typescript = { "prettier" },
               yaml = { "prettier" },
               zsh = { "shfmt" },
            },
            format_on_save = { lsp_format = "fallback", timeout_ms = 5000 },
            log_level = vim.log.levels.ERROR,
            notify_no_formatters = true,
            notify_on_error = true,
         },
         init = function(_, opts)
            vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
         end,
      },

      --- Treesitter has some post install requirements ---------------
      --- Treesitter has some post install requirements ---------------
      --- Treesitter has some post install requirements ---------------
      --- Treesitter has some post install requirements ---------------

      --- And finally the end of the lazy.nvim specs. -----------------
   },
   -- This is the scheme to use while installing, not that which we run
   install = { colorscheme = { "github_dark_high_contrast" } },

   -- automatically check for plugin updates
   checker = { enabled = true },
})

--- Set remaining options --------------------------------------------
-- And now a raft of options to bend vim to my will. Some are standard, some are
-- not. While things like mini.basics and vim-sensible exist, I want to think
-- about these as I set them. As grouping is a matter of opinion, I keep them
-- in alphabetical order.

vim.opt.autoindent = true
vim.opt.breakindent = true
vim.opt.confirm = true -- Confirm to save changes before exiting modified buffer
vim.opt.cursorline = true
vim.opt.expandtab = true
vim.opt.formatoptions = "jcroqlnt" -- tcqj
vim.opt.ignorecase = true -- Ignore case
vim.opt.inccommand = "split" -- preview incremental substitute
vim.opt.linebreak = true -- Wrap lines at convenient points
vim.opt.list = true -- Show some invisibles
vim.opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }
vim.opt.number = true -- I like them
vim.opt.relativenumber = true -- And I'm learning to like this
vim.opt.ruler = true
vim.opt.scrolloff = 3 -- Lines of context
vim.opt.shiftround = true -- Round indent
vim.opt.shortmess:append("c")
vim.opt.showcmd = true
vim.opt.showmatch = true
vim.opt.showmode = false -- status line should do this --
vim.opt.sidescrolloff = 8 -- Columns of context
vim.opt.signcolumn = "yes"
vim.opt.smartcase = true
vim.opt.splitbelow = true
vim.opt.splitkeep = "screen"
vim.opt.splitright = true
vim.opt.syntax = "on"
vim.opt.title = true
vim.opt.wildmenu = true
vim.opt.winborder = "rounded" -- others double, single, solid, shadow, none
vim.opt.winminwidth = 5 -- Minimum window width

--- Completion related, including popups -----------------------------

vim.cmd("set completeopt+=fuzzy")
vim.cmd("set completeopt+=noinsert")
vim.cmd("set completeopt+=noselect")

o.pumblend = 10
vim.cmd("hi PmenuSel blend=0")
o.pumheight = 10
o.pummaxwidth = 50
o.pumwidth = 15

--- Prose and text for humans ---------------------------------------

-- While more text/writing support is needed, for now I'll enable spelling and
-- thesaurus. Note that my dictionary additions are in my config and not under
-- stdpath(data).

vim.opt.spelllang = { "en_us" }
vim.opt.spellsuggest = "best,10"
local spelldir = vim.fn.stdpath("config") .. "/spell"
vim.opt.spellfile = spelldir .. "/en.utf-8.add"
vim.opt.thesaurus = spelldir .. "/thesaurus.txt"
