-- all my colorschemes in one file.
-- cataracts are a bitch, and mine are just forming
return {
  { "bluz71/vim-nightfly-colors", priority = 1000 },
  { "romainl/vim-malotru", priority = 1000 },
  { "romainl/vim-dichromatic", priority = 1000 },
  { "BlameTroi/amber", priority = 1000 },
  { "BlameTroi/amberchrome", priority = 1000 }, ---- amberchrome and amberchrome-phosphorechrome
  { "BlameTroi/mono-jade", priority = 1000 },
  { "BlameTroi/vim-amber", priority = 1000 },
  { "BlameTroi/vim-colors-green", priority = 1000 },
  -- { "chriskempson/base16-vim", priority = 1000 },
  { "ellisonleao/gruvbox.nvim", priority = 1000 },
  {
    "jonathanfilip/vim-lucius",
    priority = 1000,
    -- opts = function()
    --   vim.opt.background = "dark"
    --   vim.cmd([[
    -- LuciusBlackHighContrast
    -- ]])
    -- end,
  },
  -- {
  --   "rebelot/kanagawa.nvim",
  --   opts = {
  --     -- Default options:
  --     compile = false, -- enable compiling the colorscheme
  --     undercurl = true, -- enable undercurls
  --     commentStyle = { italic = true },
  --     functionStyle = {},
  --     keywordStyle = { italic = true },
  --     statementStyle = { bold = true },
  --     typeStyle = {},
  --     transparent = false, -- do not set background color
  --     dimInactive = false, -- dim inactive window `:h hl-NormalNC`
  --     terminalColors = true, -- define vim.g.terminal_color_{0,17}
  --     colors = { -- add/modify theme and palette colors
  --       palette = {},
  --       theme = { wave = {}, lotus = {}, dragon = {}, all = {} },
  --     },
  --     -- overrides = function(colors) -- add/modify highlights
  --     --    return {}
  --     -- end,
  --     theme = "dragon", -- Load "wave" theme
  --     background = { -- map the value of 'background' option to a theme
  --       dark = "dragon", -- try "wave" !
  --       light = "lotus",
  --     },
  --   },
  -- },
}
