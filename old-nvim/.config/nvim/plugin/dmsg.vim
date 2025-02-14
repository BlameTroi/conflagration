" dmsg.vim

" ===========================================================================
" a debug print helper as a global command.
"
" Troy Brumley
" blametroi@gmail.com
"  
" Copyright 2023 by Troy Brumley, all rights reserved.
" ===========================================================================

let g:DMSG_flag = 0

function! g:DMSG(txt)
  if g:DMSG_flag
    echomsg a:txt
  endif
endfunction

command! -nargs=1 DMSG       call g:DMSG(<args>)
command! -nargs=0 DMSGOn     let g:DMSG_flag = 1
command! -nargs=0 DMSGOff    let g:DMSG_flag = 1
command! -nargs=0 DMSGToggle let g:DMSG_flag = !g:DMSG_flag

" ============================================================================
" end dmsg.vim
" ============================================================================
" vim: ts=2 sts=2 sw=2 et
