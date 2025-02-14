" toggles.vim

" ===========================================================================
" toggle setting and state functions and mappings starting from
" Losh's lvsthw with additions and tweaks as i see a need.
"
" Troy Brumley
" blametroi@gmail.com
"  
" Copyright 2023 by Troy Brumley, all rights reserved.
" ===========================================================================

" ===========================================================================
" any boolean can be toggled easily enough:
" ===========================================================================

nnoremap <leader>N :setlocal number! relativenumber!<cr>

" ===========================================================================
" other options and states require more work:
"
" here's how to handle a numeric setting where 0 is the off
" state and anything else is on.
"
" i don't use folding much, but it's the example Losh used.
" ===========================================================================

"nnoremap <leader>f :call <SID>FoldColumnToggle()<cr>

"function! s:FoldColumnToggle()
"  if &foldcolumn
"    setlocal foldcolumn=0
"  else
"    setlocal foldcolumn=4
"  endif
"endfunction

" ===========================================================================
" some state requires additional wiring to work well. using the quickfix
" window as an example takes us through remembering state and then dealing
" the user changing the state without using our mapping.
"
" no solution for that here as of yet, but the use of winnr and wincmd 
" are a reasonable attempt to not break the user's window layout in vim.
"
" todo: i should at least get this playing nice with grep-operator.vim
" plugin wrt open status.
" ===========================================================================

nnoremap <leader>q :call <SID>QuickFixToggle()<cr>

let s:quickfix_is_open = 0

function! s:QuickFixToggle()
  if s:quickfix_is_open
    :cclose
    let s:quickfix_is_open = 0
    execute s:quickfix_return_to_window . "wincmd w"
  else
    let s:quickfix_return_to_window = winnr()
    :copen 5
    let s:quickfix_is_open = 1
  endif
endfunction

" ============================================================================
" end toggles.vim
" ============================================================================
" vim: ts=2 sts=2 sw=2 et
