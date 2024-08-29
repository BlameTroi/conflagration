" .vimrc
"set runtimepath^=~/.vim runtimepath+=~/.vim/after
"let &packpath = &runtimepath
"source ~/.vimrc

" init.vim

" ============================================================================
" Troy's vim configuration iteration next
"
" ideas from all over the net and github, but Steve Losh's Learn Vimscript
" the Hard Way is probably the most interesting influence. i'm running neovim
" instead of stock vim (rip Bram, your work is a force multiplier) but using
" vimscript for my customization. i've seen enough of it over the years that
" i'd rather use it than learn lua.
"
" Troy Brumley
" blametroi@gmail.com
"
" Copyright 2023 by Troy Brumley, all rights reserved.
" ============================================================================

" ============================================================================
" on windows i used '.vim' instead of 'vimfiles'; this makes synchronization
" across (heterogeneous) systems easier. i tried to use the neovim xdg_config
" approach but it's been more work than it's worth. back to .vim and .vimrc.
"
" also here's where to add in process plugins, they need t come before the
" runtime.
" ============================================================================

"if (has('win32') || has('win64'))
"  set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
"endif


" ============================================================================
" my preferred leaders... it appears to be a best practice to do this early
"
" be sure to unmap space, you'll see lags in insert mode otherwise
" ============================================================================

nnoremap <space> <nop>
let mapleader = " "
let maplocalleader="\\"


" ============================================================================
" encoding and locale
" ============================================================================

set encoding=utf-8


" ============================================================================
" integrate with system clipboard
" ============================================================================

set clipboard=unnamedplus
set mouse=nvi
set mousemodel=popup_setpos
set mousescroll=ver:0,hor:0


" ============================================================================
" basic ui preferences
" ============================================================================

set number                      " what line is this
set norelativenumber            " relative numbering is ok situationally
set signcolumn=yes              " sign column and number column
set laststatus=2

" set cursorline                  " try highlighting cursor line
" set cursorlineopt=number        " actually not finding it helpful

set noincsearch                 " not incremental, too jumpy
set hlsearch                    " but do highlight
set shortmess-=SFf              " and count matches

set showmatch                   " show matching bracket
set matchtime=1                 " default of 5 is too slow for me

set ignorecase                  " search case insensitive
set smartcase                   " unless upper case in search

set scrolljump=1                " lines to scroll when cursor leaves screen
set scrolloff=3                 " minimum lines to keep above and below cursor
set sidescroll=8                " chunks

set splitbelow                  " how i like splits
set splitright                  " how i like splits

" set virtualedit=all             " i may want this for mainframe style editing
                                  " perhaps as a filetype specific thing?

set whichwrap=b,s,h,l,<,>,[,]   " backspace and cursor keys wrap too

set wrap linebreak              " sensible for a default, cursors jkhl mapped to g...

set wildmode=longest:full,full  " wildmenu enabled by default, command <Tab> completion

set wildignore+=*.o,*.out,*.obj " files/extensions to ignore
set wildignore+=*.ppu           " compiler artifacts and hidden directories
set wildignore+=*.exe,*.pyc,*.rbc,*.rbo,*.com,*.class,*.gem
set wildignore+=.*,.git,.svn
set wildignore+=*.swp,*~,._*
set wildignore+=*.zip,*.tar.gz,*.tar.bz2,*.rar,*.tar.xz

set complete-=i                 " do not search includes, tags suffice

set noshowmode                  " display in status bar instead

set updatetime=100              " default deemed too slow for many async ops

set foldlevel=99                " open most folds


" ----------------------------------------------------------------------------
" default grep and make and maybe compile settings
"
" use unixish settings since i've got msys64 and such on windows so commands
" should mostly work the same. ripgrep is available and installed but i'll
" begin with grep. i don't have enough code to be worried about speed of
" these operations.
"
" grep:
"
" on windows grepprg defaulted to 'findstr /n $* nul' even though that is
" not the documented default. another option here would be the unix default
" of 'grep -n $* /dev/null' but more research is required.
"
" make:
"
" on windows makeprg defaulted to "make " and that may be sufficient.
"
" compiler:
"
" this is not a set option but rather selects settings for a compiler in
" a language support plugin. not doing anything here for now. 
"
" there's a lot of exclusions here on my windows system. i need to find a
" better way to manage this, but for now the removal of binary files and
" not using -R (recurse and follow symlinks) makes this managable when i
" run from ~.
"
" txb: the trailing space is intended on this line.
" ----------------------------------------------------------------------------
set grepprg=grep\ -I\ --exclude-dir={bin,.git,go,.config,.local,scoop,Downloads,.vscode,AppData,Dropbox,OneDrive}\ -n\ $\*\ 


" ============================================================================
" a quick list all <cword> found in current file command from vimtips
" ============================================================================
command! FindAll :execute 'vimgrep ' . expand('<cword>') . ' ' . expand('%') | :copen | :cc


" ============================================================================
" plugins
"
" after review i've settled back on vim-plug. i've used it before and it
" works.
" ============================================================================

" ----------------------------------------------------------------------------
" install vim-plug if it isn't already.
" ----------------------------------------------------------------------------
let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" ----------------------------------------------------------------------------
" plugin configuration, use the default plugged directory.
" ----------------------------------------------------------------------------

" plugins between plug#begin() and plug#end()
call plug#begin()

" open files where i left off
Plug 'farmergreg/vim-lastplace'

" i'm hoping gruvbox (8) light mode will come close to the plan9 acme
" experience ... it didn't, but dark gruvbox8_hard is working out well
Plug 'lifepillar/vim-gruvbox8', {'branch': 'neovim'}
Plug 'fcpg/vim-fahrenheit'

" this is for if i want to roll my own or customize a colorscheme
Plug 'lifepillar/vim-colortemplate', {'tag': 'v2.*'}

" it's either signify or gitgutter, let's try gitgutter
Plug 'airblade/vim-gitgutter'

" lightline
Plug 'itchyny/lightline.vim'

" this julia language support plugin looks to be properly laid
" out so i'm grabbing it as an example to explore.
" Plug 'JuliaEditorSupport/julia-vim'

" i'm finding that i really want a file explorer in the editor ... sigh
Plug 'preservim/nerdtree'

" old school tag management and fuzzy finding with gutentags and
" ctrlp.
" Plug 'ludovicchabant/vim-gutentags'
" Plug 'ctrlpvim/ctrlp.vim'

" my own fork to allow fpc as a synonym of pascal
" Plug 'blametroi/tagbar'

" ecumenial support
Plug 'tpope/vim-surround'
" Plug 'tpope/vim-sensible'
 
" allow modelines but limit the commands that can be used
Plug 'ciaranm/securemodelines'
let g:secure_modelines_verbose = 1
let g:secure_modelines_leave_modeline = 1

" undo tree visualization and persistence
Plug 'mbbill/undotree'

" easy alignment
Plug 'junegunn/vim-easy-align'

Plug 'robertmeta/nofrils'
" colorschemes under review
"Plug 'nlknguyen/papercolor-theme'
"Plug 'andreypopp/vim-colors-plain'
"Plug 'vim-scripts/less.vim'
"Plug 'kamwitsta/flatwhite-vim'
"Plug 'axvr/photon.vim'
"Plug 'vyshane/cleanroom-vim-color'
"Plug 'alxhnr/dark_tty'

" navigation, movement, and keys
"Plug 'liuchengxu/vim-which-key'
"Plug 'zakj/vim-showmarks'
"Plug 'justinmk/vim-sneak'
"Plug 'easymotion/vim-easymotion'

" languages

Plug 'sterpe/vim-algol68'

" go, i don't seem able to avoid it
Plug 'fatih/vim-go', {'tag': 'v1.*'}

call plug#end()
" plug#end()  sets `filetype plugin indent on` and `syntax enable`
" it's not clear if this should be before or after color setting, but
" since color schemes can be loaded above i'm assuming before.
" ============================================================================

" ============================================================================
" configure control p
" ============================================================================
let g:ctrlp_extensions = ['tag', 'buffertag', 'quickfix', 'dir', 'undo']
" maybe later add ['rtscript', 'line', 'changes', 'mixed', 'bookmarkdir']

" ============================================================================
" my preferred indenting and formatting
"
" some of these settings are defaults in nvim and/or vim-sensible, but i
" prefer to be explicit.
" ============================================================================

set copyindent                  " use tabs or spaces as on prior line
set shiftwidth=2                " use indents of two spaces
set expandtab                   " it has the virtue of being consistent
set tabstop=2                   " an indentation every two columns
set softtabstop=2               " let backspace delete indent
set autoindent                  " indent at the same level of the previous line
set backspace=indent,eol,start  " handles line breaks the way i want
set nosmartindent               " avoid snotfights with syntax plugins (i hope)
set textwidth=78                " card images will never die!
set nojoinspaces                " prevents inserting two spaces on join line

set formatoptions+=jcroql1n     " i hope this is sensible for most text
set formatoptions-=t            " no autowrap anywhere please

" ----------------------------------------------------------------------------
" filetype defaults to :filetype plugin indent on
" syntax defaults to :syntax enable
" ----------------------------------------------------------------------------

" ============================================================================
" code a chrome
"
" color and italics and such.
"
" termguicolors and guicursor settings get terminal mode (n)vim to do colors
" and highlighting as gui mode does.
"
" guicursor has many options, but just blank seems to do what i want right
" now: keep from changing the cursor to a thin vertical bar when inserting.
" ============================================================================

set termguicolors
set background=dark
"colorscheme gruvbox8_hard
colorscheme fahrenheit
"colorscheme nofrils-acme
set guicursor=
highlight Comment gui=italic cterm=italic
" sign sign everywhere a sign
highlight! link SignColumn LineNR
" marks
highlight! link ShowMarksHLl LineNR
highlight! link ShowMarksHLu LineNR
highlight! link ShowMarksHLm LineNR
highlight! link ShowMarksHLo LineNR

" ============================================================================
"  key mapping
" ============================================================================

" ----------------------------------------------------------------------------
" nvim loads matchit by default, so its mappings are enabled
" ----------------------------------------------------------------------------

" Auto center on matched string.
noremap n nzz
noremap N Nzz

" Visually select the text that was last edited/pasted (Vimcast#26).
noremap gV `[v`]

" Enable saving by `Ctrl-s`
nnoremap <C-s> :w<CR>
inoremap <C-s> <ESC>:w<CR>

" ----------------------------------------------------------------------------
" utility key mappings i'm picking up from losh's book
" ----------------------------------------------------------------------------

" edit and source my vimrc
nnoremap <leader>ev :split $MYVIMRC<cr>
nnoremap <leader>sv :source $MYVIMRC<cr>

" uppercase the word just entered (insert mode) or
" current word (normal mode)
inoremap <c-u> <esc>viwU<esc>i
nnoremap <c-u> viwU<esc>

" enclose the word just entered or current word in
" quotes or apostrophes
" todo: there's likely a plugin that will do this better
nnoremap <leader>" viw<esc>a"<esc>bi"<esc>lel
nnoremap <leader>' viw<esc>a'<esc>bi'<esc>lel

" enclose selected text in quotes, apostrophes, parens, brackets, etc.
" todo: there's likely a plugin that will do this better
vnoremap <leader>" <esc>`<i"<esc>`>i"<esc>
vnoremap <leader>' <esc>`<i'<esc>`>i'<esc>
vnoremap <leader>( <esc>`<i(<esc>`>i)<esc>
vnoremap <leader>[ <esc>`<i[<esc>`>i]<esc>
vnoremap <leader>{ <esc>`<i{<esc>`>i}<esc>

" ---------------------------------------------------------------------------
" don't use arrow keys so much
" some use alternate jk for <esc> but i actually like <esc>
" ---------------------------------------------------------------------------
"inoremap <up> <nop>
"inoremap <down> <nop>
"inoremap <left> <nop>
"inoremap <right> <nop>
"nnoremap <up> <nop>
"nnoremap <down> <nop>
"nnoremap <left> <nop>
"nnoremap <right> <nop>

" ---------------------------------------------------------------------------
" Allow easy navigation between wrapped lines.
" ---------------------------------------------------------------------------
vnoremap j gj
vnoremap k gk
nnoremap j gj
nnoremap k gk

" ---------------------------------------------------------------------------
" consistent cursor placement after copy or paste
" ---------------------------------------------------------------------------
vnoremap <silent> y y`]
nnoremap <silent> p p`]

" ---------------------------------------------------------------------------
" mapping to always use 'standard' regexp syntax for patterns
" txb: this can be problematic and it's difficult to get \v active across
"      all patterns in vim. i'll use \v a lot, but will do so explicitly
" ---------------------------------------------------------------------------
" nnoremap / /\v

" ---------------------------------------------------------------------------
" mapping to toggle display of trailing whitespace
" note that it appears that just a lone "match" is all you need to
" turn off match highlighting.
" ---------------------------------------------------------------------------
nnoremap <leader>w <cmd>match Error /\v\s+$/<cr>
nnoremap <leader>W <cmd>match<cr>

" ---------------------------------------------------------------------------
" mapping do a grep on word under cursor ... g word, G WORD
" todo: change height on copen to 25% or 33% of current available lines
" todo: was -R changed to -r to avoid some link chasing while experimenting
" ---------------------------------------------------------------------------
"nnoremap <leader>g :silent execute "grep! -r " . shellescape(expand("<cword>")) . " ."<cr>:copen 5<cr>"
"nnoremap <leader>G :silent execute "grep! -r " . shellescape(expand("<cWORD>")) . " ."<cr>:copen 5<cr>"

" ---------------------------------------------------------------------------
" map <f2> and <s-f2> to jump between locations in a quickfix list, or
" differences if in window in diff mode ... from vimtips wiki, i went looking
" for suggested keys to map for :cnext and :cprev but say this additional
" use of a ternary operator for being in diff mode and decided it was worth
" the weight.
" ---------------------------------------------------------------------------
nnoremap <expr> <silent> <f2>   (&diff ? "]c" : ":cnext\<CR>")
nnoremap <expr> <silent> <s-f2> (&diff ? "[c" : ":cprev\<cr>")

" ---------------------------------------------------------------------------
" vim-easy-align
" ---------------------------------------------------------------------------
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)
" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" ----------------------------------------------------------------------------
" from vimtips wiki, syntax highlighting group under cursor
" ----------------------------------------------------------------------------
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . "> trans<" . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" ============================================================================
" configure tagbar
" ============================================================================
let g:tagbar_position = 'botright vertical'
let g:tagbar_width = max([32, winwidth(0) / 5])

nnoremap <F8> :TagbarToggle<CR>


let NERDTreeCustomOpenArgs = {'file': {'where':'v', 'keepopen':1}, 'dir': {}}

" ============================================================================
" configure Undotree
" ============================================================================
nnoremap <F7> :UndotreeToggle<cr>

" global callback function, when undotree toggled, sets keys for only that
" buffer.
function g:Undotree_CustomMap()
  nmap <buffer> J <plug>UndotreeNextState
  nmap <buffer> K <plug>UndotreePreviousState
endfunc
" ============================================================================
" some typo correctors and "macros"
" ============================================================================

iabbrev adn and
iabbrev waht what
iabbrev teh the
iabbrev tihs this
iabbrev thsi this

" these include trailing doublespaces to break lines in markdown.
iabbrev ccopy Copyright 2023 by Troy Brumley, all rights reserved.  
iabbrev ssig Troy Brumley  <cr>blametroi@gmail.com  <cr>  So let it be written. So let it be done.  


" ============================================================================
" common autocommands
"
" some bits that either don't have or don't warrant a full plugin
" implementation
" ============================================================================

augroup vimrc_general
  autocmd!
  autocmd VimResized * wincmd =
  autocmd FileType *.txt,*.md setlocal wrap lbr
  autocmd BufReadPost,BufNewFile *.txt,*.md match hl_trailing_spaces '\v\c\s+$'
  autocmd BufReadPost,BufNewFile *.txt,*.md match hl_trailing_spaces '\v\c\.\s\s'
augroup END

highlight link hl_trailing_spaces PMENUSEL

" add my own 'todo' highlights here instead of stuffing them in
" filetype plugins
augroup vimrc_txb_hl
  autocmd!
  autocmd Syntax * syntax match hl_txb "\<txb\:\=" contained containedin=.*Comment
  autocmd Syntax * syntax match hl_txb "\<nb\:\=" contained containedin=.*Comment
  autocmd BufReadPost,BufNewFile *.vim match hl_typo '\v\c<(if|while).*\s\=\s'
augroup END

highlight link hl_txb Todo
highlight link hl_typo Error


" ============================================================================
" end .vimrc
" ============================================================================
" vim: ts=2 sts=2 sw=2 et
