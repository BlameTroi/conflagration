-- ~/.config/mininvim/lua/config/plumbing.lua

-- Plumbing and infrastructure for thins like Tree Sitter grammars, binary
-- helper management, and LSP.

local add = MiniDeps.add

-- Mason manages external tools for formatting, linting, and LSP support.

add({ source = "mason-org/mason.nvim" })
require("mason").setup({
   ui = {
      icons = {
         package_installed = "✓",
         package_pending = "➜",
         package_uninstalled = "✗",
      },
   },
})

-- Treesitter improves code highlights, editing, and navigation. It is built in,
-- but grammars need to be generated locally.
--
-- Ruby has been mentioned as a special case in every turnkey config I have
-- seen. I've preserved its exceptions for now but at some point I need to see
-- they are still relevant.

add({
   source = "nvim-treesitter/nvim-treesitter",
   hooks = { post_checkout = function() vim.cmd("TSUpdate") end },
})
add({   source =  "nvim-treesitter/nvim-treesitter-textobjects" })

-- I'm only using a few languages, let's make sure they are all installed.
-- TODO: are the modules correct. Especially Textobjects?

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
      -- keymaps = {
      --    init_selection = "gnn", -- set to `false` to disable one of the mappings
      --    node_incremental = "grn",
      --    scope_incremental = "grc",
      --    node_decremental = "grm",
      -- },
   },
   textobjects = { enable = true, lookahead = true },
   indent = { enable = true, disable = { "ruby" } },
})

-- Language servers. This is the basic configuration. Neovim's LSP support will
-- load configurations as needed from "lsp" in the runtime path. Note that the
-- names here are the files (or require modules, if you will) from which each
-- langauge server is configured.
--
-- These are *NOT* the binary names.

add({ source = "neovim/nvim-lspconfig" })
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

-- Lazydev helps link Lua libraries to your workspace. I attempt to use this
-- just to quite down the varlus lint warnings. TODO: Perhaps move to luals.lua?

add({ source = "folke/lazydev.nvim" })
require("lazydev").setup({
   ft = "lua",
   { path = "${3rd}/luv/library", words = "vim%.uv" },
})

-- Conform manages source code formatters. Use Mason to get the appropriate
-- binaries.

add({ source = "stevearc/conform.nvim" })
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
