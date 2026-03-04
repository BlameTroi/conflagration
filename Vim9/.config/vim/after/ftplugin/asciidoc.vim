" Sets undo points to sentences or phrases. Commas and semicolons
" maybe? This should be text documentation only.

inoremap . .<C-g>u
inoremap ! !<C-g>u
inoremap ? ?<C-g>u
inoremap : :<C-g>u
inoremap ; ;<C-g>u

" Document formating.

setl ts=5 sw=5 sts=5 et ai fdm=manual
setl linebreak nonumber spell fo+=tn21j fo-=croq tw=72 wrap

" This is teting for soft breaking paragraphs, w means wrap at space and it
" should prevent titles and the like from getting munged up.

setl fo+=w
