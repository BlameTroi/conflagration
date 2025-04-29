local M = {
  "BlameTroi/512-words",
  vim.keymap.set("n", "gW", function()
    require("512-words").open()
  end),
  opts = {
    buffer = {
      textwidth = 80,
    },
    words = 0x100,
    storage_directory = "~/Notepad",
    file_extension = ".md",
  },
}
return M
