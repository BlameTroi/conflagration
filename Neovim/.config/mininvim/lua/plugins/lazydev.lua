-- ~/.config/mininvim/lua/plugins/lazydev.lua

local add = MiniDeps.add

-- Lazydev helps link Lua libraries to your workspace. I attempt to use this
-- just to quite down the varlus lint warnings.
-- TODO: Perhaps move to luals.lua?
-- NOTE: This currently throws a deprecated warning on client notify. Folke is
--       on vacation so he can't fix it atm.
add({ source = "folke/lazydev.nvim" })
require("lazydev").setup({
   ft = "lua",
   { path = "${3rd}/luv/library", words = "vim%.uv" },
})
