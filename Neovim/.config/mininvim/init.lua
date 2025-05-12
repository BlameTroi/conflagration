-- ~/.config/mininvim/init.lua
--
-- A trial configuration using mini.nvim. Lazy is giving me problems, I'm not
-- interested in Plug or some other prebuilt kit. That said, I can assemble a
-- configuration with the tools from mini.
--
-- While mini.deps has fewer options than lazy.nvim, it seems much cleaner and
-- easier to work with.
--
-- I had a working and understandable configuration in an afternoon. Kickstart,
-- Lazyvim, and Lazy package manager are very brittle. This is on me and not
-- the authors--I don't grok Lua and Neovim's architecture well enough to
-- modify those setups.
--
-- TODO:
--
-- 2) Go modular.
-- 3) Fix signcolumn and markers.
-- 5) See commented out code for diagnostic, when it is uncommented there are
--    messages "no next diagnostic to go to". Seems related to the number of
--    TS parsers loaded. Search for NOTE:
--
-- Bootstrap mini.nvim so we can use mini.deps. As with Lazy and pretty much
-- any package management I've seen in Emacs and (Neo)Vim, the first thing you
-- do is install the manager in a one-off manner. Here we clone the full
-- mini.nvim repository. We will later pick and choose among the modules after
-- mini.deps is running.
--
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

require("mini.deps").setup({ path = { package = path_package } })

-- Readability shortcuts for mini.dps functions.
-- I expect mini.deps.setup() to have set MiniDeps.
-- T is a global for me to stash things in. Everyone else seems to use G
-- for such things.

local add, now, later = MiniDeps.add, MiniDeps.now, MiniDeps.later
T = { add, now, later }

-- My configuration is broken up into modules, with high level groups
-- run in what I believe to be the correct order. But those details are
-- delegated.
--
-- The distinction between "config", "plugins", and "mappings" is roughly:
--
--   config: Neovim options and infrastructure. These are the items that
--           it is reasonable to expect to be established in any Neovim
--           environment. There is some leeway as to which plugins to
--           use for some features (example: mini.clue or which-key) but
--           the feature is expected.
--
--  plugins: The things that make truly customize Neovim for my use, or
--           that would not be expected in a standard configuration.
--
-- mappings: I do mappings _after_ load and setup of functionality.
--
-- This line between "config" and "plugins" isn't always clear, but idealy
-- "config" won't be touched except for breaking changes in Neovim or the
-- plugins it manages.

require("config")
require("plugins")
require("mappings")

-- The author uses separate add/now/later blocks, which makes sense to me.
-- If I read this right, now before add before later, in source sequence.
-- Adds are always wrapped in now and later from what I see.

-- UI related.

-- Now that the UI themeing and such as established it's time for
-- functionality. First I'll get all the mini.* modules I want.

-- General Workflow Modules:

later(function() require("mini.pick").setup() end)

-- And now I can add my non-mini plugins.

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

-- A very good directory and file diff.

later(
   function()
      add({
         source = "ZSaberLv0/zfvimdirdiff",
         depends = {
            "ZSaberLV0/ZFVimJob",
            "ZSaberLv0/ZFVimIgnore",
            "ZSaberLv0/ZFVimBackup",
         },
      })
   end
)

-- Much more to come.
