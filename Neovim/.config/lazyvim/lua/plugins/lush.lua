return {
  -- {"meain/vim-colorswitch"},
  -- { "amadeus/vim-convert-color-to" },
  {
    "BlameTroi/nvim-colorizer.lua",
    opts = {
      lua = { rgb_fn = true, hsl_fn = true },
      vim = { rgb_fn = true, hsl_fn = true },
    },
  },
  { "rktjmp/lush.nvim" },
}
