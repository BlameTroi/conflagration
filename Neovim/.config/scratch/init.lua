-- ~/.config/mininvim/lua/config/options.lua

---Uncomment below to double check that your only undefined globals
---are those you trust: eg, vim.*, MiniDeps.*.
--- diagnostic disable:undefined-global

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

--- Set leader and local leader before any other plugins ------------

-- Make sure to setup `mapleader` and `maplocalleader` before
-- loading lazy.nvim so that mappings are correct.

vim.cmd("nnoremap <space> <nop>")
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
vim.opt.background = "dark"

--- Set leader and local leader before any other plugins -------------

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

      --- Several standard zero-configuration plugins -----------------

      { "folke/which-key.nvim", lazy = true },

      { "nvim-tree/nvim-web-devicons", lazy = true },

      --- Treesitter has some post install requirements ---------------

      -- Most of this is copied out of lazyvim's definition.
      -- See lazyvim/lua/lazyvim/plugins/treesitter.lua.

      {
         "nvim-treesitter/nvim-treesitter",
         build = ":TSUpdate",
         event = {  "VeryLazy" },
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
      event = "VeryLazy",
      opts = {
         formatters_by_ft = {
            bash = { "shfmt" },
            c = { "clang-format" },
            fortran = { "fprettify" },
            javascript = { "prettier" },
            json = { "prettier" },
            lua = { "stylua", opts = { args = "--search-parent-directories" } },
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
      config = function(_, opts)
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

-- I hate meeces to pieces. In terminal vim all I want the mouse to do
-- is return focus to a window. Don't move the pointer position.

vim.opt.mouse = "" -- mouse clicks shouldn't move the cursor

-- And now a raft of options to bend vim to my will. Some are standard, some are
-- not.
--
-- There may be some duplicates in here. I have made a pass to remove them but
-- I'm only human.

vim.opt.confirm = true -- Confirm to save changes before exiting modified buffer
vim.opt.formatoptions = "jcroqlnt" -- tcqj
vim.opt.ignorecase = true -- Ignore case
vim.opt.inccommand = "split" -- preview incremental substitute
vim.opt.linebreak = true -- Wrap lines at convenient points
vim.opt.list = true -- Show some invisibles
vim.opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }
vim.opt.number = true -- I like them
vim.opt.relativenumber = true -- And I'm learning to like this
vim.opt.scrolloff = 3 -- Lines of context
vim.opt.shiftround = true -- Round indent
vim.opt.sidescrolloff = 8 -- Columns of context
vim.opt.splitkeep = "screen"
vim.opt.winminwidth = 5 -- Minimum window width
vim.opt.clipboard = "unnamedplus" -- share with system clipboard

-- While more text/writing support is needed, for now I'll enable spelling and
-- thesaurus. Note that my dictionary additions are in my config and not under
-- stdpath(data).

vim.opt.spelllang = { "en_us" }
vim.opt.spellsuggest = "best,10"
local spelldir = vim.fn.stdpath("config") .. "/spell"
vim.opt.spellfile = spelldir .. "/en.utf-8.add"
vim.opt.thesaurus = spelldir .. "/thesaurus.txt"
