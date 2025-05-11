-- ~/.config/mininvim/lua/config/options.lua

-- Set various options and defaults that don't require any plugins besides the
-- mini suite to be loaded. In here I only use mini.basics (sensible defaults)
-- and then do my overrides.
--
-- You could stop initializaton after this and should have a working but
-- primitive system.

-- The first thing to do is to set leader and localleader. As I prefer space for
-- leader, we need to force space in normal mode to be a no-op. It is normally a
-- synonym for 'l'.
--
-- NOTE: Always set leader and mapleader before starting any plugin that might
-- create a key mapping.

vim.cmd("nnoremap <space> <nop>")
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- Next mini.basics applies a set of common default settings (a la Tim Pope's
-- vim-sensible and others). While mini.deps won't update anything I've already
-- set, I prefer to run it first.
--
-- Just in case.

require("mini.basics").setup({
   options = { basic = true, extra_ui = true, win_borders = "single" },
   mappings = {
      basic = true,
      option_toggle_prefix = "",
      windows = true,
      move_with_alt = true,
   },
   autocommands = { basic = true, relnum_in_visual_mode = false },
   silent = true,
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
