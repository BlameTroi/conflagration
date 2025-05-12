-- ~/.config/mininvim/lua/config/editing.lua

---Uncomment below to double check that your only undefined globals
---are those you trust: eg, vim.*, MiniDeps.*.
---@diagnostic disable:undefined-global

-- Basic Vim editing extensions for things such as movement, pairing,
-- and so forth.

-- Extend around and include text objects.
require("mini.ai").setup()

-- Commenting helpers.
require("mini.comment").setup()

-- Smarter handing of paired characters.
require("mini.pairs").setup()

-- Split function arguments to separate lines, or rejoin them.
require("mini.splitjoin").setup()

-- Text Completion.
require("mini.completion").setup()

-- Infer indents and tabs in the buffer from the file loaded.
MiniDeps.add({ source = "tpope/vim-sleuth" })
