return {
  --  "BlameTroi/512-words",
  dir = "~/Projects/Neovim/512-words",
  vim.keymap.set("n", "gW", function()
    require("512-words").open()
  end),
  opts = {
    buffer = {
      textwidth = 75,
    },
    words = 0x100,
    storage_directory = "~/Notepad",
    date_prefix = "#",
    file_extension = ".md",
  },
}
