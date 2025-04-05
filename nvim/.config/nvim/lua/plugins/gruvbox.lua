return {
  -- add gruvbox
  {
    "ellisonleao/gruvbox.nvim",
    init = function()
      --vim.o.background = "light"
      --vim.o.contrast = "hard"
      --vim.o.cursorline = true
      vim.o.number = true
      vim.o.termguicolors = true
    end,
    opts = {
      show_warnings = true,
    },
  },

  -- Configure LazyVim to load gruvbox
  {
    "LazyVim/LazyVim",
    opts = {
      background = "light",
      contrast = "hard",
      colorscheme = "gruvbox",
    },
  },
}
