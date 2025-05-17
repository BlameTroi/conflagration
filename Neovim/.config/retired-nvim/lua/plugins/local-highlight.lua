local M = {
   "tzachar/local-highlight.nvim",
   config = function()
      require("local-highlight").setup {
         file_types = {
            "scheme",
            "c",
            "lua",
            "racket",
            "fortran",
            "f90",
            "pascal",
         }, -- If this is given only attach to this
         hlgroup = "LocalHighlight",
         cw_hlgroup = nil,
         insert_mode = false,
         min_match_len = 1,
         max_match_len = math.huge,
         highlight_single_match = true,
         animate = {
            enabled = false, -- requires snacks, and I don't need animation
         },
         debounce_timeout = 200,
      }
   end,
}
return M
