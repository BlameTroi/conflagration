-- ~/.config/mininvim/lua/plugins/lazydev.lua

---Uncomment below to double check that your only undefined globals
---are those you trust: eg, vim.*, MiniDeps.*.
---@diagnostic disable:undefined-global

-- 512 words (actually 256 for me) daily journaling.

MiniDeps.add({ source = "BlameTroi/512-words" })
require("512-words").setup({
   buffer = { textwidth = 75 },
   words = 0x100,
   storage_directory = "~/Notepad",
   date_prefix = "#",
   file_extension = ".md",
})
vim.keymap.set("n", "gW", function() require("512-words").open() end)
