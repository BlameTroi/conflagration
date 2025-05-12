-- ~/.config/mininvim/lua/config/motions.lua

-- Jumping about in Vim.

-- Move by "brackets". ] next or [ prior -- buffer, coment block, file, etc.
require("mini.bracketed").setup()

-- Jump to a next or prior single character.
require("mini.jump").setup()

-- Jump to any character in two characters.
require("mini.jump2d").setup()
