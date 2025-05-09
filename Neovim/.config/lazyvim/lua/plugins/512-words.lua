local M = {
  --  "BlameTroi/512-words",
  dir = "~/Projects/Neovim/512-words",
  vim.keymap.set("n", "gW", function()
    require("512-words").open()
  end),
  config = function()
    require("512-words").setup({
      buffer = { textwidth = 75 },
      words = 0x100,
      storage_directory = "~/Notepad",
      date_prefix = "#",
      file_extension = ".md",
    })
  end,
}
return M
