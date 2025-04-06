local M = { -- Highlight, edit, and navigate code
   'nvim-treesitter/nvim-treesitter',
   build = ':TSUpdate',
   main = 'nvim-treesitter.configs', -- Sets main module to use for opts
   -- [[ Configure Treesitter ]] See `:help nvim-treesitter`
   opts = {
      ensure_installed = { 'bash', 'c', 'lua', 'luadoc', 'markdown', 'vim', 'vimdoc' },
      -- Autoinstall languages that are not installed
      auto_install = true,
      highlight = {
         enable = true,
         -- Some languages depend on vim's regex highlighting system (such as Ruby) for indent rules.
         --  If you are experiencing weird indenting issues, add the language to
         --  the list of additional_vim_regex_highlighting and disabled languages for indent.
         additional_vim_regex_highlighting = { 'ruby' },
      },
      incremental_selection = {
         enable = true,
         keymaps = {
            init_selection = "gnn", -- set to `false` to disable one of the mappings
            node_incremental = "grn",
            scope_incremental = "grc",
            node_decremental = "grm",
         },
      },
      indent = { enable = true, disable = { 'ruby' } },
   },
}
return M
