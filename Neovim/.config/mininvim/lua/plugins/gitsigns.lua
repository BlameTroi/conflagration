-- ~/.config/mininvim/lua/plugins/gitsigns.lua

---Uncomment below to double check that your only undefined globals
---are those you trust: eg, vim.*, MiniDeps.*.
---@diagnostic disable:undefined-global

local add = MiniDeps.add
add({ source = "lewis6991/gitsigns.nvim" })
require("gitsigns").setup()
