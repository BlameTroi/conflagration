-- ~/.config/mininvim/lua/plugins/files.lua

---Uncomment below to double check that your only undefined globals
---are those you trust: eg, vim.*, MiniDeps.*.
---@diagnostic disable:undefined-global

-- Files and Directories. For now I'm just using Neotree.

MiniDeps.add({
   source = "nvim-neo-tree/neo-tree.nvim",
   depends = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
      "MunifTanjim/nui.nvim",
      -- {"3rd/image.nvim", opts = {}}, -- Optional image support in preview window: See `# Preview Mode` for more information
   },
})
