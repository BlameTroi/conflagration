-- ~/.config/mininvim/lua/config/visuals.lua

-- As I have the beginnings of cataracts, I am always look for a good scheme
-- with high contrast and a color set that doesn't offend.

local add, now, later = MiniDeps.add, MiniDeps.now, MiniDeps.later

vim.opt.background = "dark"

add({ source = "projekt0n/github-nvim-theme" })

require("github-theme").setup({
   options = {
      styles = {
         comments = "italic",
         keywords = "bold",
         types = "italic,bold",
      },
   },
})

vim.cmd("colorscheme github_dark_high_contrast")

-- Customize visuals.

require("mini.notify").setup()
require("mini.icons").setup()
require("mini.statusline").setup() -- TODO: review what's in the status line

--- Highlight important comments. This mini module also highlights hex color
--- patterns in text in the color specified. #808080 is grey, for example.

local hipatterns = require("mini.hipatterns")
hipatterns.setup({
   highlighters = {
      fixme = { pattern = "BUG:", group = "MiniHipatternsFixme" },
      hack = { pattern = "HACK:", group = "MiniHipatternsHack" },
      todo = { pattern = "TODO:", group = "MiniHipatternsTodo" },
      note = { pattern = "NOTE:", group = "MiniHipatternsNote" },
      -- TODO: do this only for filetypes that it matters, lua, vim, css
      hex_color = hipatterns.gen_highlighter.hex_color(),
   },
})

-- Display keymap help.

later(function()
   add({ source = "folke/which-key.nvim" })
   require("which-key").setup({
      opts = {
         keys = {
            "<leader>?",
            function() require("which-key").show({ global = false }) end,
            desc = "Buffer Local Keymaps (which-key)",
         },
         -- preset = "helix",
         win = { border = "single" },
      },
   })
end)
