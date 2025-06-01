--- ~/.config/kickstart/lua/config/autocmds/qb64.lua

-- Basic is QB64
local qb64group = vim.api.nvim_create_augroup("qb64", {
  clear = true,
})

vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
  desc = "Basic becomes QB64",
  group = qb64group,
  pattern = { "*.bas", "*.bi", "*.bm", "*.BAS", "*.BI", "*.BM" },
  callback = function ()
    vim.cmd([[
 let g:qb64dev_qb64_directory = "/Users/troi/Projects/Basic/qb64pe/"
 let g:qb64dev_autofind_qb64 = 0
 nnoremap <F5> :call qb64dev#QB64CompileAndRun()<cr>
 nnoremap <F11> :call qb64dev#QB64Compile()<cr>
 ]])
  end,
})
