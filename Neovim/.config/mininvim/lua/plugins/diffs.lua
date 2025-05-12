-- ~/.config/mininvim/lua/plugins/lazydev.lua

---Uncomment below to double check that your only undefined globals
---are those you trust: eg, vim.*, MiniDeps.*.
---@diagnostic disable:undefined-global

-- This is the diff tool I find most useful.

MiniDeps.add({
   source = "ZSaberLv0/zfvimdirdiff",
   depends = {
      "ZSaberLV0/ZFVimJob",
      "ZSaberLv0/ZFVimIgnore",
      "ZSaberLv0/ZFVimBackup",
   },
})
