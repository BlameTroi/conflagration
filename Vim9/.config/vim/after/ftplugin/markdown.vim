" Sets undo points to sentences or phrases. Commas and semicolons
" maybe? This should be text documentation only.

inoremap . .<C-g>u
inoremap ! !<C-g>u
inoremap ? ?<C-g>u
inoremap : :<C-g>u
inoremap ; ;<C-g>u

" Document formating.

setl ts=5 sw=5 sts=5 et ai fdm=manual
setl linebreak nonumber spell fo+=tn21j fo-=croq tw=70 wrap

