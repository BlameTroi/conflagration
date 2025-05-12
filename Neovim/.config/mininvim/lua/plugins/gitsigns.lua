-- ~/.config/mininvim/lua/plugins/gitsigns.lua

local add = MiniDeps.add
add({ source = "lewis6991/gitsigns.nvim" })
require("gitsigns").setup()
