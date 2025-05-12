-- ~/.config/mininvim/lua/config/editing.lua

-- Basic Vim editing extensions for things such as movement, pairing,
-- and so forth.

local add = MiniDeps.add

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
add({ source = "tpope/vim-sleuth" })
