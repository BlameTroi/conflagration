local M = {
   "nvim-lualine/lualine.nvim",
   dependencies = { "nvim-tree/nvim-web-devicons", "jonathanfilip/vim-lucius" },
   opts = {
      options = {
         icons_enabled = true,
         component_separators = "|",
         section_separators = "",
      },
   },
}
return M
