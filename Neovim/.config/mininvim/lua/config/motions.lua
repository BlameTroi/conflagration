-- ~/.config/mininvim/lua/config/motions.lua

---Uncomment below to double check that your only undefined globals
---are those you trust: eg, vim.*, MiniDeps.*.
---@diagnostic disable:undefined-global

-- Jumping about in Vim.

-- Move by "brackets". ] next or [ prior -- buffer, coment block, file, etc.

require("mini.bracketed").setup()

-- Jump to a next or prior single character.

require("mini.jump").setup()

-- Jump to any character in two characters.

require("mini.jump2d").setup()

-- Reopen a file at its last edit location. Note that this plugin is not being
-- maintained but it still works fine.

MiniDeps.add({ source = "ethanholz/nvim-lastplace" })
require("nvim-lastplace").setup({
   lastplace_ignore_buftype = { "quickfix", "nofile", "help" },
   lastplace_ignore_filetype = {
      "gitcommit",
      "gitrebase",
      "svn",
      "hgcommit",
   },
   lastplace_open_folds = true,
})
