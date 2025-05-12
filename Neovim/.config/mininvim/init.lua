-- ~/.config/mininvim/init.lua
--
-- A trial configuration using mini.nvim. Lazy is giving me problems, I'm not
-- interested in Plug, nor in some other prebuilt kit. That said, I can assemble
-- a configuration with the tools from mini.
--
-- While mini.deps has fewer options than lazy.nvim, it seems much cleaner and
-- easier to work with.
--
-- I had a working and understandable configuration in an afternoon. Kickstart,
-- Lazyvim, and Lazy package manager are very brittle. This is on me and not
-- the authors--I don't grok Lua and Neovim's architecture well enough to modify
-- those setups.
--
-- TODO: See commented out code for diagnostic, when it is uncommented there are
-- messages "no next diagnostic to go to". Seems related to the number of TS
-- parsers loaded. Search for NOTE:

--- Boostrap our package manager -- mini.deps ---------------------------------
--
-- Bootstrap mini.nvim so we can use mini.deps. As with Lazy and pretty much
-- any package management I've seen in Emacs and (Neo)Vim, the first thing you
-- do is install the manager in a one-off manner. Here we clone the full
-- mini.nvim repository. We will later pick and choose among the modules after
-- mini.deps is running.
--
-- Mini uses git for downloading and packadd to lay the plugins into the
-- configuration directories. Even though most people don't use packadd, I
-- find the idea of using a standard part of Vim apealling.
--
-- Mini.deps sets a global MiniDeps. I use this throughout.

---Uncomment below to double check that your only undefined globals
---are those you trust: eg, vim.*, MiniDeps.*.
---@diagnostic disable:undefined-global

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

--- Three stage configuration -- config, plugins, and mappings ----------------
--
-- My configuration is broken up into modules, with high level groups
-- run in what I believe to be the correct order. But those details are
-- delegated.
--
-- The distinction between "config", "plugins", and "mappings" is roughly:
--
--   config: Neovim options and infrastructure. These are the items that
--           it is reasonable to expect to be established in any Neovim
--           environment. There is some leeway as to which plugins to
--           use for some features (example: mini.clue or which-key)
--           but the feature is expected.
--
--  plugins: The things that make truly customize Neovim for my use, or
--           that would not be expected in a standard configuration.
--
-- mappings: I do mappings _after_ load and setup of functionality.
--
-- The dividing line between "config" and "plugins" isn't always clear, but
-- ideally "config" won't be touched except for breaking changes in Neovim
-- or base plugins such as mini, mason, conform, treesitter, and lsp.
--
-- Remember, requiring a directory name runs the init.lua in that
-- directory. That init decides what to do next.

require("config")
require("plugins")
require("mappings")
