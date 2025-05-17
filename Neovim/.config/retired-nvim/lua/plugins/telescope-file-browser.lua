local M = {
   "nvim-telescope/telescope-file-browser.nvim",
   dependencies = {
      "nvim-telescope/telescope.nvim",
      "nvim-lua/plenary.nvim"
   },
    vim.keymap.set(
      "n",
      "<space>fb",
      "<cmd>Telescope file_browser<CR>",
      { noremap = true }
    )
}
return M
