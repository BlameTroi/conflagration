-- ~/.config/mininvim/lua/config/plumbing.lua

---Uncomment below to double check that your only undefined globals
---are those you trust: eg, vim.*, MiniDeps.*.
---@diagnostic disable:undefined-global

-- Plumbing and infrastructure for things like Tree Sitter grammars,
-- linters, formatters, and LSP.

-- Treesitter improves code highlights, editing, and navigation. It is built in,
-- but grammars need to be generated locally.
--
-- Ruby has been mentioned as a special case in every turnkey config I have
-- seen. I've preserved its exceptions for now but at some point I need to see
-- they are still relevant.
--
-- Grammars can be loaded or upgraded interactively via :TSInstall.
--
-- Grammar binaries are in config("data") in /parser under the nvim-treesitter
-- package as shared libraries. The source used to build them does not appear to
-- be preserved.

MiniDeps.add({
   source = "nvim-treesitter/nvim-treesitter",
   hooks = { post_checkout = function() vim.cmd("TSUpdate") end },
})
require("nvim-treesitter.configs").setup({
   ensure_installed = {
      "bash",
      "c",
      "fortran",
      "lua",
      "markdown",
      "vim",
      "vimdoc",
      "python",
      "odin",
      "ruby",
   },
   auto_install = true,
   ignore_install = {},
   sync_install = false,
   modules = {},
   highlight = {
      enable = true,
      additional_vim_regex_highlighting = { "ruby" },
   },
   incremental_selection = {
      enable = true,
      -- TODO: keymaps really need to be cleaned and consolidated
      -- keymaps = {
      --    init_selection = "gnn", -- set to `false` to disable one of the mappings
      --    node_incremental = "grn",
      --    scope_incremental = "grc",
      --    node_decremental = "grm",
      -- },
   },
   -- textobjects = { enable = true, lookahead = true },
   indent = { enable = true, disable = { "ruby" } },
})

-- Mason and Lspconfig are intertwined. These three packages work together to
-- simplify managing the external tools for linting, formatting, and providing
-- LSP support. The order of configuration is important, and the sections that
-- follow are as according to the documentation under mason.nvim:
--
-- mason -> lspconfig -> mason-lspconfig.

MiniDeps.add({ source = "mason-org/mason.nvim" })
MiniDeps.add({ source = "neovim/nvim-lspconfig" })
MiniDeps.add({ source = "mason-org/mason-lspconfig.nvim" })

-- Mason can download and install external tools for linting, formatting, and to
-- provide LSP support. These are installed under config("data")/mason. This
-- path is searched for binaries, not your shell path.

require("mason").setup({
   ui = {
      icons = {
         package_installed = "✓",
         package_pending = "➜",
         package_uninstalled = "✗",
      },
   },
})

-- Language servers. This is the basic configuration. Neovim's LSP support will
-- load configurations as needed from "/lsp" in the runtime path. Note that the
-- names here are the files (or require modules, if you will) from which each
-- langauge server is configured.
--
-- These are *NOT* the binary names.

vim.lsp.enable({
   "bashls",
   "clangd",
   "fortls",
   "gopls",
   "jsonls",
   "luals",
   "marksman",
   "millet",
   "ols",
   "rubocop",
   "ruff",
   "taplo",
   "textlsp",
   "tsls",
   "vimls",
   "yamlls",
})

-- Mason-lspconfig will automatically enable installed servers. I'm not too
-- worried about startup time yet, so we'll just get them all.
--
-- Enabling is done by default. I included the value to be explicit.

require("mason-lspconfig").setup({
   automatic_enable = true,
})

-- Conform manages source code formatters. Use Mason to get the appropriate
-- binaries. I begin to wish we Neovim took a language first approach to
-- configuration. Maybe I can make some sort of cap if I ever get proficient
-- with Lua.
--
-- I have conform set to format_on_save.
--
-- The formatexpr being set to conform should prevent LSP from getting in the
-- way if I have a formatter configured. It is expected that the conform
-- formatexpr will fall back to the lsp formatexpr which should fall back to
-- normal Vim formatting.

MiniDeps.add({ source = "stevearc/conform.nvim" })
require("conform").setup({
   formatters_by_ft = {
      bash = { "shfmt" },
      c = { "clang-format" },
      fortran = { "fprettify" },
      javascript = { "prettier" },
      json = { "prettier" },
      lua = { "stylua", opts = { args = "--search-parent-directories" } },
      markdown = { "prettier" },
      python = { "ruff" },
      ruby = { "rubocop" },
      toml = { "taplo" },
      typescript = { "prettier" },
      yaml = { "prettier" },
      zsh = { "shfmt" },
   },
   format_on_save = { lsp_format = "fallback", timeout_ms = 5000 },
   log_level = vim.log.levels.ERROR,
   notify_no_formatters = true,
   notify_on_error = true,
})
vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
