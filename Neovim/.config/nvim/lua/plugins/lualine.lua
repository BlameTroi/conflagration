local M = {
   "nvim-lualine/lualine.nvim",
   dependencies = { "nvim-tree/nvim-web-devicons" },
   opts = {
      options = {
         --         theme = "gruvbox_dark",
         icons_enabled = true,
         component_separators = "|",
         section_separators = "",
      },
   },
}
return M
