-- ~/.config/mininvim/init.lua

-- A trial configuration using mini.nvim. Lazy is giving me problems, I'm not
-- interested in Plug or some other prebuilt kit. That said, I can assemble a
-- configuration with the tools from mini.
--
-- While mini.deps has fewer options than lazy.nvim, it seems much cleaner and
-- easier to work with.

-- TODO:
-- DONE 1) Mason & LSP
-- 2) Go modular.
-- 3) Fix signcolumn and markers.
-- 4) Any additional formatter?
-- 5) See commented out code for diagnostic, when it is uncommented there are
--    messages "no next diagnostic to go to". Seems related to the number of
--    TS parsers loaded. Search for NOTE:

-- Bootstrap mini.nvim so we can use mini.deps. As with Lazy and pretty much
-- any package management I've seen in Emacs and (Neo)Vim, the first thing you
-- do is install the manager in a one-off manner. Here we clone the full
-- mini.nvim repository. We will later pick and choose among the modules after
-- mini.deps is running.

-- Mini uses git for downloading and packadd to lay the plugins into the
-- configuration directories. Even though most people don't use packadd, I find
-- the idea of using a standard part of Vim apealling.

local path_package = vim.fn.stdpath("data") .. "/site"
local mini_path = path_package .. "/pack/deps/start/mini.nvim"
if not vim.loop.fs_stat(mini_path) then
  vim.cmd('echo "Installing `mini.nvim`" | redraw')
  local clone_cmd = {
    "git",
    "clone",
    "--filter=blob:none",
    -- Uncomment next line to use 'stable' branch
    -- '--branch', 'stable',
    "https://github.com/echasnovski/mini.nvim",
    mini_path,
  }
  vim.fn.system(clone_cmd)
  vim.cmd("packadd mini.nvim | helptags ALL")
end

-- Fire up mini.deps.

require("mini.deps").setup({ path = { package = path_package } })

-- Readability shortcuts for mini.dps functions.
-- I expect setup() to have set MiniDeps.

local add, now, later = MiniDeps.add, MiniDeps.now, MiniDeps.later

-- All my standard option settings. Leader and mapleader must be set before any plugins that might create keymaps based
-- on leader or mapleader. Add any additional options here

now(function()
  -- The first thing to do is to set leader and localleader. As I prefer space for leader, we need to force space in
  -- normal mode to be a no-op. It is normally a synonym for 'l'.
  vim.cmd("nnoremap <space> <nop>")
  vim.g.mapleader = " "
  vim.g.maplocalleader = "\\"

  -- Now get mini.basics applied for "sensible" defaults.
  require("mini.basics").setup({
    options = {
      basic = true,
      extra_ui = true,
      win_borders = "single",
    },
    mappings = {
      basic = true,
      option_toggle_prefix = "",
      windows = true,
      move_with_alt = true,
    },
    autocommands = {
      basic = true,
      relnum_in_visual_mode = false,
    },
    silent = true,
  })

  -- I'm peculiar about the mouse.
  vim.opt.clipboard = "unnamedplus"
  vim.opt.background = "dark" -- hello darkness my old friend
  vim.opt.mouse = "" -- mouse clicks shouldn't move the cursor

  -- These are options that are either different from the defaults or I want different from what mini.basics will set.
  -- While the mini.basics doc says it not change an option that has been changed already, I'm placing most of these
  -- _after_ mini.basics setup.
  --
  -- I've made a serious effort to remove duplicates for defaults/mini.basics values, but I'm only human so there could
  -- be redundancies.
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

  -- The basic spell checker has been pretty good so far. I keep my dictionary in
  -- a non-standard location -- my actual config. The default is under `stdpath("data")`.
    vim.opt.spelllang = { "en_us" }
    vim.opt.spellsuggest = "best,10"
    local spelldir = vim.fn.stdpath("config").."/spell"
    vim.opt.spellfile = spelldir .. "/en.utf-8.add"
    vim.opt.thesaurus = spelldir .. "/thesaurus.txt"
end)

-- The author uses separate add/now/later blocks, which makes sense to me.
-- If I read this right, now before add before later, in source sequence.
-- Adds are always wrapped in now and later from what I see.

-- UI related.
now(function() require("mini.notify").setup() end)

now(function() require("mini.icons").setup() end)

now(function() require("mini.statusline").setup() end)

now(function()
  local hipatterns = require("mini.hipatterns")
  hipatterns.setup({
    highlighters = {
      fixme = { pattern = "BUG:", group = "MiniHipatternsFixme" },
      hack = { pattern = "HACK:", group = "MiniHipatternsHack" },
      todo = { pattern = "TODO:", group = "MiniHipatternsTodo" },
      note = { pattern = "NOTE:", group = "MiniHipatternsNote" },
      -- TODO: do this only for filetypes that it matters, lua, vim, css
      hex_color = hipatterns.gen_highlighter.hex_color(),
    },
  })
end)

now(function() require("mini.git").setup() end)

now(function() require("mini.diff").setup() end)

now(function()
  add({ source = "projekt0n/github-nvim-theme" })
  require("github-theme").setup({
    options = {
      styles = {
        comments = "italic",
        keywords = "bold",
        types = "italic,bold",
      },
    },
  })
  vim.opt.background = "dark"
  vim.cmd("colorscheme github_dark_high_contrast")
end)

-- Now that the UI themeing and such as established it's time for
-- functionality. First I'll get all the mini.* modules I want.

-- Editor Modules:

now(function() require("mini.ai").setup() end)
now(function() require("mini.comment").setup() end)
now(function() require("mini.completion").setup() end)
now(function() require("mini.keymap").setup() end)
now(function() require("mini.move").setup() end)
now(function() require("mini.operators").setup() end)
now(function() require("mini.pairs").setup() end)
now(function() require("mini.splitjoin").setup() end)
now(function() require("mini.surround").setup() end)

-- General Workflow Modules:

now(function() require("mini.bracketed").setup() end)
now(function() require("mini.clue").setup() end)
now(function() require("mini.jump").setup() end)
now(function() require("mini.jump2d").setup() end)
now(function() require("mini.pick").setup() end)

-- And now I can add my non-mini plugins.

-- Return to the last position in a file. Note that this plugin is not
-- being maintained but it still works fine.

now(function()
  add({ source = "ethanholz/nvim-lastplace" })
  require("nvim-lastplace").setup({
    lastplace_ignore_buftype = { "quickfix", "nofile", "help" },
    lastplace_ignore_filetype = { "gitcommit", "gitrebase", "svn", "hgcommit" },
    lastplace_open_folds = true,
  })
end)

-- Treesitter to highlight, edit, and navigate code.

now(function()
  add({
    source = "nvim-treesitter/nvim-treesitter",
    hooks = { post_checkout = function() vim.cmd("TSUpdate") end },
  })
  add({ source = "nvim-treesitter/nvim-treesitter-textobjects" })

  require("nvim-treesitter.configs").setup({
    ensure_installed = { "bash", "c", "lua", "markdown", "vim", "vimdoc", "python", "odin", "ruby" },
    auto_install = true,
    ignore_install = {},
    sync_install = false,
    modules = {},
    highlight = {
      enable = true,
      additional_vim_regex_highlighting = { "ruby" },
    },
    incremental_selection = {
      enable = true,
      -- keymaps = {
      --    init_selection = "gnn", -- set to `false` to disable one of the mappings
      --    node_incremental = "grn",
      --    scope_incremental = "grc",
      --    node_decremental = "grm",
      -- },
    },
    textobjects = { enable = true },
    indent = { enable = true, disable = { "ruby" } },
  })
end)

-- Lazydev helps link things up to reduce lint warnings.

now(function()
  add({ source = "folke/lazydev.nvim" })
  require("lazydev").setup({
    ft = "lua",
    { path = "${3rd}/luv/library", words = "vim%.uv" },
  })
end)

-- Mason for managing LSP executables.

now(function()
  add({ source = "mason-org/mason.nvim" })
  require("mason").setup({
    ui = {
      icons = {
        package_installed = "✓",
        package_pending = "➜",
        package_uninstalled = "✗",
      },
    },
  })
end)

-- Formatting =================================================================
now(function()
  add({ source = "stevearc/conform.nvim" })

  require("conform").setup({
    -- Map of filetype to formatters
    formatters_by_ft = {
      javascript = { "prettier" },
      json = { "prettier" },
      lua = { "stylua" },
      python = { "black" },
    },
  })
end)

-- Language server configurations =============================================
now(function()
  add({ source = "neovim/nvim-lspconfig" })

  -- All language servers are expected to be installed with 'mason.vnim'
  vim.lsp.enable({
    "clangd",
    "lua_ls",
    "pyright",
    "ols",
    "rust_analyzer",
    "ts_ls",
  })
end)

-- Neotree, I prefer the old style tree view.

later(function()
  add({
    source = "nvim-neo-tree/neo-tree.nvim",
    depends = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
      "MunifTanjim/nui.nvim",
      -- {"3rd/image.nvim", opts = {}}, -- Optional image support in preview window: See `# Preview Mode` for more information
    },
  })
end)

-- 512 words (actually 256 for me) daily journaling.

later(function()
  add({ source = "BlameTroi/512-words" })
  require("512-words").setup({
    buffer = { textwidth = 75 },
    words = 0x100,
    storage_directory = "~/Notepad",
    date_prefix = "#",
    file_extension = ".md",
  })
  vim.keymap.set("n", "gW", function() require("512-words").open() end)
end)

-- Infer indents and tabs in the buffer from the file loaded.

later(function() add({ source = "tpope/vim-sleuth" }) end)

-- A very good directory and file diff.

later(
  function()
    add({
      source = "ZSaberLv0/zfvimdirdiff",
      depends = { "ZSaberLV0/ZFVimJob", "ZSaberLv0/ZFVimIgnore", "ZSaberLv0/ZFVimBackup" },
    })
  end
)

-- Treesitter
-- LSP

-- I'm trying mini.clue instead of whichkey. There's a need for configuration to
-- advise mini.clue as to which keys should trigger a clue.

now(function()
  local miniclue = require("mini.clue")
  miniclue.setup({
    triggers = {
      -- Leader triggers
      { mode = "n", keys = "<Leader>" },
      { mode = "x", keys = "<Leader>" },

      -- Built-in completion
      { mode = "i", keys = "<C-x>" },

      -- `g` key
      { mode = "n", keys = "g" },
      { mode = "x", keys = "g" },

      -- Marks
      { mode = "n", keys = "'" },
      { mode = "n", keys = "`" },
      { mode = "x", keys = "'" },
      { mode = "x", keys = "`" },

      -- Registers
      { mode = "n", keys = '"' },
      { mode = "x", keys = '"' },
      { mode = "i", keys = "<C-r>" },
      { mode = "c", keys = "<C-r>" },

      -- Window commands
      { mode = "n", keys = "<C-w>" },

      -- `z` key
      { mode = "n", keys = "z" },
      { mode = "x", keys = "z" },
    },

    clues = {
      -- Enhance this by adding descriptions for <Leader> mapping groups
      miniclue.gen_clues.builtin_completion(),
      miniclue.gen_clues.g(),
      miniclue.gen_clues.marks(),
      miniclue.gen_clues.registers(),
      miniclue.gen_clues.windows(),
      miniclue.gen_clues.z(),
    },
    -- Delay before showing clue window
    delay = 300,

    -- Keys to scroll inside the clue window
    scroll_down = "<C-d>",
    scroll_up = "<C-u>",
  })
end)

-- Highlight TODO

-- Much more to come.

-- Keymaps come last. The first large block of maps is exracted from LazyVim's
-- defaults. Many of those were assigned to Snacks functions, but I'm not using
-- Snacks, so I've commented them out for reference.
-- TODO: consider switching to mini.keymaps

local map = vim.keymap.set

-- For some reason <space> is not working for <leader>. Somewhere in the canned
-- kits I was using they must be clearing <space> in normal mode for it to
-- work. I think like this:

-- map("n", "<space>", "", { noremap = true})

-- Which-key interogate local map.
map("n", "<leader>?", function() require("which-key").show({ global = false }) end, { desc = "Buffer Local Keymaps" })
-- map("n", "<leader>bo", function() Snacks.bufdelete.other() end, { desc = "Delete Other Buffers" })
-- better up/down
map({ "n", "x" }, "j", "v:count == 0 ? 'gj' : 'j'", { desc = "Down", expr = true, silent = true })
map({ "n", "x" }, "<Down>", "v:count == 0 ? 'gj' : 'j'", { desc = "Down", expr = true, silent = true })
map({ "n", "x" }, "k", "v:count == 0 ? 'gk' : 'k'", { desc = "Up", expr = true, silent = true })
map({ "n", "x" }, "<Up>", "v:count == 0 ? 'gk' : 'k'", { desc = "Up", expr = true, silent = true })

-- Move to window using the <ctrl> hjkl keys
map("n", "<C-h>", "<C-w>h", { desc = "Go to Left Window", remap = true })
map("n", "<C-j>", "<C-w>j", { desc = "Go to Lower Window", remap = true })
map("n", "<C-k>", "<C-w>k", { desc = "Go to Upper Window", remap = true })
map("n", "<C-l>", "<C-w>l", { desc = "Go to Right Window", remap = true })

-- Resize window using <ctrl> arrow keys
map("n", "<C-Up>", "<cmd>resize +2<cr>", { desc = "Increase Window Height" })
map("n", "<C-Down>", "<cmd>resize -2<cr>", { desc = "Decrease Window Height" })
map("n", "<C-Left>", "<cmd>vertical resize -2<cr>", { desc = "Decrease Window Width" })
map("n", "<C-Right>", "<cmd>vertical resize +2<cr>", { desc = "Increase Window Width" })

-- Move Lines
map("n", "<A-j>", "<cmd>execute 'move .+' . v:count1<cr>==", { desc = "Move Down" })
map("n", "<A-k>", "<cmd>execute 'move .-' . (v:count1 + 1)<cr>==", { desc = "Move Up" })
map("i", "<A-j>", "<esc><cmd>m .+1<cr>==gi", { desc = "Move Down" })
map("i", "<A-k>", "<esc><cmd>m .-2<cr>==gi", { desc = "Move Up" })
map("v", "<A-j>", ":<C-u>execute \"'<,'>move '>+\" . v:count1<cr>gv=gv", { desc = "Move Down" })
map("v", "<A-k>", ":<C-u>execute \"'<,'>move '<-\" . (v:count1 + 1)<cr>gv=gv", { desc = "Move Up" })

-- buffers
map("n", "<S-h>", "<cmd>bprevious<cr>", { desc = "Prev Buffer" })
map("n", "<S-l>", "<cmd>bnext<cr>", { desc = "Next Buffer" })
map("n", "[b", "<cmd>bprevious<cr>", { desc = "Prev Buffer" })
map("n", "]b", "<cmd>bnext<cr>", { desc = "Next Buffer" })
map("n", "<leader>bb", "<cmd>e #<cr>", { desc = "Switch to Other Buffer" })
map("n", "<leader>`", "<cmd>e #<cr>", { desc = "Switch to Other Buffer" })
-- map("n", "<leader>bd", function() Snacks.bufdelete() end, { desc = "Delete Buffer" })
-- map("n", "<leader>bo", function() Snacks.bufdelete.other() end, { desc = "Delete Other Buffers" })
map("n", "<leader>bD", "<cmd>:bd<cr>", { desc = "Delete Buffer and Window" })

-- Clear search and stop snippet on escape
-- map({ "i", "n", "s" }, "<esc>", function()
--   vim.cmd("noh")
--   --LazyVim.cmp.actions.snippet_stop()
--   return "<esc>"
-- end, { expr = true, desc = "Escape and Clear hlsearch" })

-- Clear search, diff update and redraw
-- taken from runtime/lua/_editor.lua
map(
  "n",
  "<leader>ur",
  "<Cmd>nohlsearch<Bar>diffupdate<Bar>normal! <C-L><CR>",
  { desc = "Redraw / Clear hlsearch / Diff Update" }
)

-- https://github.com/mhinz/vim-galore#saner-behavior-of-n-and-n
map("n", "n", "'Nn'[v:searchforward].'zv'", { expr = true, desc = "Next Search Result" })
map("x", "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next Search Result" })
map("o", "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next Search Result" })
map("n", "N", "'nN'[v:searchforward].'zv'", { expr = true, desc = "Prev Search Result" })
map("x", "N", "'nN'[v:searchforward]", { expr = true, desc = "Prev Search Result" })
map("o", "N", "'nN'[v:searchforward]", { expr = true, desc = "Prev Search Result" })

-- Add undo break-points
map("i", ",", ",<c-g>u")
map("i", ".", ".<c-g>u")
map("i", ";", ";<c-g>u")

-- save file
map({ "i", "x", "n", "s" }, "<C-s>", "<cmd>w<cr><esc>", { desc = "Save File" })

--keywordprg
map("n", "<leader>K", "<cmd>norm! K<cr>", { desc = "Keywordprg" })

-- better indenting
map("v", "<", "<gv")
map("v", ">", ">gv")

-- commenting
map("n", "gco", "o<esc>Vcx<esc><cmd>normal gcc<cr>fxa<bs>", { desc = "Add Comment Below" })
map("n", "gcO", "O<esc>Vcx<esc><cmd>normal gcc<cr>fxa<bs>", { desc = "Add Comment Above" })

-- lazy
map("n", "<leader>l", "<cmd>Lazy<cr>", { desc = "Lazy" })

-- new file
map("n", "<leader>fn", "<cmd>enew<cr>", { desc = "New File" })

-- location list
map("n", "<leader>xl", function()
  local success, err = pcall(vim.fn.getloclist(0, { winid = 0 }).winid ~= 0 and vim.cmd.lclose or vim.cmd.lopen)
  if not success and err then vim.notify(err, vim.log.levels.ERROR) end
end, { desc = "Location List" })

-- quickfix list
map("n", "<leader>xq", function()
  local success, err = pcall(vim.fn.getqflist({ winid = 0 }).winid ~= 0 and vim.cmd.cclose or vim.cmd.copen)
  if not success and err then vim.notify(err, vim.log.levels.ERROR) end
end, { desc = "Quickfix List" })

map("n", "[q", vim.cmd.cprev, { desc = "Previous Quickfix" })
map("n", "]q", vim.cmd.cnext, { desc = "Next Quickfix" })

-- formatting
map({ "n", "v" }, "<leader>cf", function() LazyVim.format({ force = true }) end, { desc = "Format" })

-- -- diagnostic
-- NOTE: When I uncomment out this code block, some messages get logged during
--       initialization. It may be related to TS.
-- local diagnostic_goto = function(next, severity)
--   local go = next and vim.diagnostic.jump({ count = 1, float = true })
--     or vim.diagnostic.jump({ count = -1, float = true })
--   severity = severity and vim.diagnostic.severity[severity] or nil
--   return function() go({ severity = severity }) end
-- end
-- map("n", "<leader>cd", vim.diagnostic.open_float, { desc = "Line Diagnostics" })
-- map("n", "]d", diagnostic_goto(true), { desc = "Next Diagnostic" })
-- map("n", "[d", diagnostic_goto(false), { desc = "Prev Diagnostic" })
-- map("n", "]e", diagnostic_goto(true, "ERROR"), { desc = "Next Error" })
-- map("n", "[e", diagnostic_goto(false, "ERROR"), { desc = "Prev Error" })
-- map("n", "]w", diagnostic_goto(true, "WARN"), { desc = "Next Warning" })
-- map("n", "[w", diagnostic_goto(false, "WARN"), { desc = "Prev Warning" })

-- stylua: ignore start

-- quit
map("n", "<leader>qq", "<cmd>qa<cr>", { desc = "Quit All" })

-- highlights under cursor
map("n", "<leader>ui", vim.show_pos, { desc = "Inspect Pos" })
map("n", "<leader>uI", function() vim.treesitter.inspect_tree() vim.api.nvim_input("I") end, { desc = "Inspect Tree" })

--
-- Terminal Mappings
map("t", "<C-/>", "<cmd>close<cr>", { desc = "Hide Terminal" })
map("t", "<c-_>", "<cmd>close<cr>", { desc = "which_key_ignore" })

-- windows
map("n", "<leader>-", "<C-W>s", { desc = "Split Window Below", remap = true })
map("n", "<leader>|", "<C-W>v", { desc = "Split Window Right", remap = true })
map("n", "<leader>wd", "<C-W>c", { desc = "Delete Window", remap = true })

-- tabs
map("n", "<leader><tab>l", "<cmd>tablast<cr>", { desc = "Last Tab" })
map("n", "<leader><tab>o", "<cmd>tabonly<cr>", { desc = "Close Other Tabs" })
map("n", "<leader><tab>f", "<cmd>tabfirst<cr>", { desc = "First Tab" })
map("n", "<leader><tab><tab>", "<cmd>tabnew<cr>", { desc = "New Tab" })
map("n", "<leader><tab>]", "<cmd>tabnext<cr>", { desc = "Next Tab" })
map("n", "<leader><tab>d", "<cmd>tabclose<cr>", { desc = "Close Tab" })
map("n", "<leader><tab>[", "<cmd>tabprevious<cr>", { desc = "Previous Tab" })

-- native snippets. only needed on < 0.11, as 0.11 creates these by default
if vim.fn.has("nvim-0.11") == 0 then
  map("s", "<Tab>", function()
    return vim.snippet.active({ direction = 1 }) and "<cmd>lua vim.snippet.jump(1)<cr>" or "<Tab>"
  end, { expr = true, desc = "Jump Next" })
  map({ "i", "s" }, "<S-Tab>", function()
    return vim.snippet.active({ direction = -1 }) and "<cmd>lua vim.snippet.jump(-1)<cr>" or "<S-Tab>"
  end, { expr = true, desc = "Jump Previous" })
end
-- Add any additional keymaps here

-- Exit terminal mode in the builtin terminal with an easier to remember
-- shortcut.

map("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })

-- I turn off arrow keys in normal mode to avoid mouse/touchpad scrolling. They
-- are still on for insert and visual mode, but I may someday be able to turn
-- them and the mouse completely off in Neovim.

map("n", "<left>", "")
map("n", "<right>", "")
map("n", "<up>", "")
map("n", "<down>", "")

-- Trying to learn to not use arrow keys in insert or visual mode. The
-- power users jump out and use motion commands.

map("i", "<left>", "")
map("i", "<right>", "")
map("i", "<up>", "")
map("i", "<down>", "")

map("v", "<left>", "")
map("v", "<right>", "")
map("v", "<up>", "")
map("v", "<down>", "")
