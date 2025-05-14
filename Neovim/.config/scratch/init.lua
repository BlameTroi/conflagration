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
local km = vim.keymap

vim.cmd("nnoremap <space> <nop>")
g.mapleader = " "
g.maplocalleader = "\\"
o.mouse = "" -- mouse clicks shouldn't move the cursor
o.termguicolors = true
o.background = "dark"
g.have_nerd_font = true
o.updatetime = 250 -- Shorten some keyboard checks.
o.timeoutlen = 300

-- Schedule this setting after `UiEnter` because it can increase startup-time.
vim.schedule(function() o.clipboard = "unnamedplus" end)

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

      --- More bling ---------------------------------------------------

      { "nvim-tree/nvim-web-devicons", lazy = true },

      {
         "nvim-lualine/lualine.nvim",
         dependencies = { "nvim-tree/nvim-web-devicons" },
         opts = {
            sections = {
               lualine_x = {
                  {
                     -- I don't like the icon choices.
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

      { "folke/which-key.nvim", event = "VeryLazy", opts = { preset = "helix" } },

      --- Git diffs and hunks and stats -------------------------------

      { "lewis6991/gitsigns.nvim", event = "VeryLazy" },

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
         config = function(_, opts) require("nvim-treesitter.configs").setup(opts) end,
      },

      -- This is paired with nvim-treesitter, see comments there.

      { "nvim-treesitter/nvim-treesitter-textobjects", event = "VeryLazy" },

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
         --- formatexpr! ---
         init = function(_, opts) vim.o.formatexpr = "v:lua.require'conform'.formatexpr()" end,
      },

      --- Resume editing where I left off -----------------------------

      {
         "ethanholz/nvim-lastplace",
         opts = {
            lastplace_ignore_bufftype = { "quickfix", "nofile", "help" },
            lastplace_ignore_filetype = { "gitcommit", "gitrebase" },
            lastplace_open_folds = true,
         },
      },

      --- Treesitter has some post install requirements ---------------
      --- Treesitter has some post install requirements ---------------
      --- Treesitter has some post install requirements ---------------

      --- 512 words (actually 256 for me) daily journaling ------------

      {
         "BlameTroi/512-words",
         opts = {
            buffer = { textwidth = 75 },
            words = 0x100,
            storage_directory = "~/Notepad",
            date_prefix = "#",
            file_extension = ".md",
         },
         init = function(_, opts)
            vim.keymap.set("n", "gW", function() require("512-words").open() end)
         end,
      },

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
-- in alphabetical order at first and maybe regroup once I'm done tweaking.

o.autoindent = true
o.breakindent = true
o.completeopt = "menuone,noselect" -- needs review
o.confirm = true -- Confirm to save changes before exiting modified buffer
o.cursorline = true
o.expandtab = true
-- TODO: check format options, maybe gjl1? don't format comments???
o.formatoptions = "jcroqlnt" -- tcqj
o.ignorecase = true -- Ignore case
o.inccommand = "split" -- preview incremental substitute
o.incsearch = true -- highlight partials
o.infercase = true -- helps completion
o.linebreak = true -- Wrap lines at convenient points
o.list = true -- Show some invisibles
o.listchars = { tab = "» ", trail = "·", nbsp = "␣" }
o.number = true -- I like them
o.relativenumber = true -- And I'm learning to like this
o.ruler = false -- status line should handle this
o.scrolloff = 3 -- Lines of context
o.shiftround = true -- Round indent
o.shortmess:append("WcC") -- reduce noise
o.showcmd = true
o.showmatch = true
o.showmode = false -- status line should do this --
o.sidescrolloff = 8 -- Columns of context
o.signcolumn = "yes"
o.smartcase = true
o.smartindent = true
o.splitbelow = true
o.splitkeep = "screen"
o.splitright = true
o.syntax = "on"
o.title = true
o.wildmenu = true
o.winborder = "rounded" -- others double, single, solid, shadow, none
o.winminwidth = 5 -- Minimum window width
o.wrap = false

-- Completion related, including popups -----------------------------

vim.cmd("set completeopt+=fuzzy")
vim.cmd("set completeopt+=noinsert")
vim.cmd("set completeopt+=noselect")

o.pumblend = 10
vim.cmd("hi PmenuSel blend=0")
o.pumheight = 10
o.pummaxwidth = 50
o.pumwidth = 15
o.winblend = 10

-- Prose and text for humans ---------------------------------------

-- While more text/writing support is needed, for now I'll enable spelling and
-- thesaurus. Note that my dictionary additions are in my config and not under
-- stdpath(data).

o.spelllang = { "en_us" }
o.spellsuggest = "best,10"
local spelldir = vim.fn.stdpath("config") .. "/spell"
o.spellfile = spelldir .. "/en.utf-8.add"
o.thesaurus = spelldir .. "/thesaurus.txt"

--- Mappings --------------------------------------------------------

-- I borrow heavily from others, in particular mini.basics. Good clear code
-- there. I prefer to keep all my mappings in one place, so there's copy and
-- paste reuse here.

-- Window navigation

km.set("n", "<C-H>", "<C-w>h", { desc = "Focus on left window" })
km.set("n", "<C-J>", "<C-w>j", { desc = "Focus on below window" })
km.set("n", "<C-K>", "<C-w>k", { desc = "Focus on above window" })
km.set("n", "<C-L>", "<C-w>l", { desc = "Focus on right window" })

-- Window resize (respecting `v:count`)
km.set(
   "n",
   "<C-Left>",
   '"<Cmd>vertical resize -" . v:count1 . "<CR>"',
   { expr = true, replace_keycodes = false, desc = "Decrease window width" }
)
km.set(
   "n",
   "<C-Down>",
   '"<Cmd>resize -"          . v:count1 . "<CR>"',
   { expr = true, replace_keycodes = false, desc = "Decrease window height" }
)
km.set(
   "n",
   "<C-Up>",
   '"<Cmd>resize +"          . v:count1 . "<CR>"',
   { expr = true, replace_keycodes = false, desc = "Increase window height" }
)
km.set(
   "n",
   "<C-Right>",
   '"<Cmd>vertical resize +" . v:count1 . "<CR>"',
   { expr = true, replace_keycodes = false, desc = "Increase window width" }
)

--- Intuitive line movement in a buffer -------------------------------

-- Move by visible lines. Notes:
-- - Don't map in Operator-pending mode because it severely changes behavior:
--   like `dj` on non-wrapped line will not delete it.
-- - Condition on `v:count == 0` to allow easier use of relative line numbers.
km.set({ "n", "x" }, "j", [[v:count == 0 ? 'gj' : 'j']], { expr = true })
km.set({ "n", "x" }, "k", [[v:count == 0 ? 'gk' : 'k']], { expr = true })

-- <A-hjkl> as alternatives to arrows for cursor movement. I prefer to use the
-- <A-hjkl> for moving lines in a buffer.

km.set("n", "<A-j>", "<cmd>execute 'move .+' . v:count1<cr>==", { desc = "Move Down" })
km.set("n", "<A-k>", "<cmd>execute 'move .-' . (v:count1 + 1)<cr>==", { desc = "Move Up" })
km.set("i", "<A-j>", "<esc><cmd>m .+1<cr>==gi", { desc = "Move Down" })
km.set("i", "<A-k>", "<esc><cmd>m .-2<cr>==gi", { desc = "Move Up" })
km.set("v", "<A-j>", ":<C-u>execute \"'<,'>move '>+\" . v:count1<cr>gv=gv", { desc = "Move Down" })
km.set("v", "<A-k>", ":<C-u>execute \"'<,'>move '<-\" . (v:count1 + 1)<cr>gv=gv", { desc = "Move Up" })

-- Reselect latest changed/put/yanked

km.set(
   "n",
   "gV",
   '"`[" . strpart(getregtype(), 0, 1) . "`]"',
   { expr = true, replace_keycodes = false, desc = "Visually select changed text" }
)

-- Search inside visually highlighted text. Use `silent = false` for it to
-- make effect immediately.

km.set("x", "g/", "<esc>/\\%V", { silent = false, desc = "Search inside visual selection" })

--- Better search next/previous mappings ------------------------------

-- Instead of trying to remember different keys for different search directions,
-- n always searches next forward, N always searches previous backward.

km.set("n", "n", "'Nn'[v:searchforward].'zv'", { expr = true, desc = "Next Search Result" })
km.set("x", "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next Search Result" })
km.set("o", "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next Search Result" })
km.set("n", "N", "'nN'[v:searchforward].'zv'", { expr = true, desc = "Prev Search Result" })
km.set("x", "N", "'nN'[v:searchforward]", { expr = true, desc = "Prev Search Result" })
km.set("o", "N", "'nN'[v:searchforward]", { expr = true, desc = "Prev Search Result" })

--- Save file using the CUA standard key ------------------------------

km.set({ "i", "x", "n", "s" }, "<C-s>", "<cmd>w<cr><esc>", { desc = "Save File" })

--- Tell me about the word under the cursor ---------------------------

-- LSP support steals K, moving it from :keywordprg to vim.lsp.buf.hover(). Use
-- <leader>K for :keywordprg. I have to do some work to use it for more than
-- just :man.

km.set("n", "<leader>K", "<cmd>norm! K<cr>", { desc = "Keywordprg" })

--- Open location list ------------------------------------------------

km.set("n", "<leader>xl", function()
   local success, err = pcall(vim.fn.getloclist(0, { winid = 0 }).winid ~= 0 and vim.cmd.lclose or vim.cmd.lopen)
   if not success and err then vim.notify(err, vim.log.levels.ERROR) end
end, { desc = "Location List" })

--- Open quickfix list ------------------------------------------------

km.set("n", "<leader>xq", function()
   local success, err = pcall(vim.fn.getqflist({ winid = 0 }).winid ~= 0 and vim.cmd.cclose or vim.cmd.copen)
   if not success and err then vim.notify(err, vim.log.levels.ERROR) end
end, { desc = "Quickfix List" })

--- Treesitter information on item under cursor------------------------

km.set("n", "<leader>ui", vim.show_pos, { desc = "Inspect Pos" })
km.set("n", "<leader>uI", function()
   vim.treesitter.inspect_tree()
   vim.api.nvim_input("I")
end, { desc = "Inspect Tree" })

--- Window splits -----------------------------------------------------

km.set("n", "<leader>-", "<C-W>s", { desc = "Split Window Below", remap = true })
km.set("n", "<leader>|", "<C-W>v", { desc = "Split Window Right", remap = true })
km.set("n", "<leader>wd", "<C-W>c", { desc = "Delete Window", remap = true })

--- Terminal Mappings -------------------------------------------------

km.set("t", "<C-/>", "<cmd>close<cr>", { desc = "Hide Terminal" })
km.set("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })

--- "Hard" mode Mappings -------------------------------------------------

-- Turn off arrow keys in normal, input, and visual modes. The uber vimmers
-- do this to demonstrate superiority. I do it avoid inadvertant touchpad
-- clicks moving the cursor or changing a selection. But yeah, maybe it will
-- make me less un-uber.

km.set({ "n", "i", "v" }, "<left>", "")
km.set({ "n", "i", "v" }, "<right>", "")
km.set({ "n", "i", "v" }, "<up>", "")
km.set({ "n", "i", "v" }, "<down>", "")

--- Utility autocommands -------------------------------------------------

local gr = vim.api.nvim_create_augroup("my-init", {})

local au = function(event, pattern, callback, desc)
   vim.api.nvim_create_autocmd(event, { group = gr, pattern = pattern, callback = callback, desc = desc })
end

au("TextYankPost", "*", function() vim.highlight.on_yank() end, "Highlight yanked tet")
