" grep-operator.vim
 
" ===========================================================================
" code from working through Losh's lvsthw with additins and tweaks as i
" see a need or idea.
"
" Troy Brumley
" blametroi@gmail.com
"  
" Copyright 2023 by Troy Brumley, all rights reserved. 
" ===========================================================================

" ===========================================================================
" todo: i should at least get this playing nice with my toggle.vim
" plugin wrt open status.
" ===========================================================================

nnoremap <leader>g :set operatorfunc=<SID>GrepOperator<cr>g@
vnoremap <leader>g :<c-u>call <SID>GrepOperator(visualmode())<cr>

function! s:GrepOperator(type)
  let saved_unnamed_register = @@
  if a:type ==# "v"
    execute "normal! `<v`>y"
  elseif a:type ==# "char"
    execute "normal! `[y`]"
  else
    return
  endif
  silent execute "grep! -r " . shellescape(@@) . " ."
  copen 5
  let @@ = saved_unnamed_register
endfunction

" ============================================================================
" end grep-operator.vim
" ============================================================================
" vim: ts=2 sts=2 sw=2 et
