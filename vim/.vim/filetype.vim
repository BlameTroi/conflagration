" filetype handling additions

" Listen very carefully, I will say this only once
if exists("did_load_filetypes")
  finish
endif

" BASIC or Visual Basic
" txb--------------------------------------
" the default filetype.vim forces an assumption
" that any *.bas file that doesn't look like a
" vb file is a 'basic' file.
" txb--------------------------------------
augroup filetypedetect
  au! BufRead,BufNewFile *.bas setf basic
augroup END

