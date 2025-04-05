"
" Troy's .vimrc Environment
"
" Adding more customizations after working through the Losh "hardway" book
" and reviewing jmoyers' gist on setting up a C dev environment with
" vim and tmux.
"
" I use neovim for a faster editing experience. I only use vimscript plugins
" so I can revert at any time, but the clean and shiny nvim executable works
" for my use case.
"
set nocompatible        " Must be first

" Set shell on non window systems
if !(has('win32') || has('win64'))
  set shell=/usr/bin/zsh
endif

" On Windows, also use '.vim' instead of 'vimfiles'; this makes synchronization
" across (heterogeneous) systems easier.
if (has('win32') || has('win64'))
  set runtimepath=$HOME/.vim,$VIM/vimfiles,$VIMRUNTIME,$VIM/vimfiles/after,$HOME/.vim/after
endif

set encoding=utf-8

"
" optional packages in vim
packadd! matchit

"
" Vim-Plug
"call plug#begin('~/.vim/plugged')
"
"" Code completion
"" 
"" I thought about using ycm but as it doesn't support neovim well yet.
"" I'm also seeing issues with jedi-vim, neovim, and virtual environments
"" that I don't see with vim 8 or 9. Rather than crawl into the rabbit hole
"" of neovim I am not using virtual environments. This is hobby code and
"" while virtual environments are indeed a good thing (TM) I have no real
"" need.
"
"" speed up deaing with folds
"Plug 'Konfekt/FastFold'
"
"" helper
"Plug 'junegunn/vim-easy-align'
"
"" Python
"" ale and python-mode seem to be what the cool kids use, but they aren't
"" working well for me. I'm just going to get myself something that works
"" on the few things I need.
"Plug 'tmhedberg/SimpylFold',          {'for': 'python'}
"Plug 'Vimjas/vim-python-pep8-indent', {'for': 'python'}
"Plug 'davidhalter/jedi-vim',          {'for': 'python'}
"
""
"" Snippets
"Plug 'honza/vim-snippets'
"Plug 'SirVer/ultisnips'
"
"" Status line
"Plug 'KaraMCC/vim-streamline'
"let g:streamline_enable_devicons = 1
"let g:streamline_minimal_ui      = 0
"
"" Better vim behavior
"Plug 'ctrlpvim/ctrlp.vim'
"let g:ctrlp_custom_ignore = '\.git\|node_modules\|\.cache'
"
"Plug 'farmergreg/vim-lastplace'
"
"" allow modelines but limit the commands that can be used
"Plug 'ciaranm/securemodelines'
"let g:secure_modelines_verbose = 1
"let g:secure_modelines_leave_modeline = 1
"
"" git
"Plug 'airblade/vim-gitgutter'
"let g:gitgutter_set_sign_backgrounds = 1
"
"" tags
"Plug 'preservim/tagbar'
"Plug 'ludovicchabant/vim-gutentags'
"
"" motion and such
"Plug 'easymotion/vim-easymotion'
"Plug 'tpope/vim-surround'
"Plug 'tpope/vim-commentary'
"
"" fuzzy and grep
"Plug 'junegunn/fzf', {'do': './install --all'}
"Plug 'junegunn/fzf.vim'
"
"" TODO: qf ?
"" https://github.com/romainl/vim-qf
"
"" eye candy
"Plug 'ryanoasis/vim-devicons'
"Plug 'chrisbra/unicode.vim'
"Plug 'altercation/vim-colors-solarized'
"Plug '~/projects/vim/vim-retroi'
"
""
"" algol
"Plug 'sterpe/vim-algol68'
"
""
"Plug 'habamax/vim-pire'
"
"call plug#end()
"
"
" Enable filetype customization 
syntax enable
filetype plugin indent on

"
" Colorschemes and fonts
"
" fix from git kitty issues 108
" vim hardcodes background color erase even if the terminfo file does
" not contain bce (not to mention that libvte based terminals
" incorrectly contain bce in their terminfo files). This causes
" incorrect background rendering when using a color theme with a
" background color.
if &term == 'xterm-kitty'
  let &t_ut=''
endif

" coloring and highlighting
set background=dark
colorscheme retrobox
" colorscheme darkblue
" colorscheme retroi-amber
set termguicolors
highlight Comment gui=italic cterm=italic
highlight! link SignColumn LineNR

" nvim hack, guicursor applies to terminal mode as well???
" this disables changing the cursor to a thin vertical bar
" when in insert mode
set guicursor=  

"
" Mouse available in either vim or nvim
set mouse=a
"if !has('nvim')
"    set ttymouse=xterm2
"endif
"if has('nvim')
"    set mouse=a
"endif

"
" UI appearance and some behavior
set autowrite                   " so i don't forget
set backspace=indent,eol,start  " Backspace for dummies
set cmdheight=2                 " more space in command line
set colorcolumn=80              " hollerith wins again
set foldenable                  " Auto fold code
set foldlevelstart=99           " open most folds when starting
set foldmethod=indent           " indent makes the most sense to me
set hidden                      "
set hlsearch                    " Highlight search terms
set ignorecase                  " Case insensitive search
set incsearch                   " Find as you type search
set laststatus=2                " Always show status lines
set linespace=0                 " No extra spaces between rows
set nocursorline                " Don't highlight current line
set noshowmode                  " Statusline displays mode
set nowrap                      " Do not wrap long lines
set number                      " Line numbers on
set numberwidth=6               " xedit legacy
set ruler                       " Show the ruler
set scrolljump=1                " Lines to scroll when cursor leaves screen
set scrolloff=3                 " Minimum lines to keep above and below cursor
set showcmd                     " Show partial commands in status line and
set showmatch                   " Show matching brackets/parenthesis
set sidescroll=8                " chunks
set smartcase                   " Case sensitive when uc present
set splitbelow                  " How I like splits
set splitright                  " How I like splits
set tabpagemax=5                " Only show 5 tabs
" set virtualedit=all           " i may want this for mainframe style editing
set whichwrap=b,s,h,l,<,>,[,]   " Backspace and cursor keys wrap too
set wildmenu                    " Show list instead of just completing
set wildmode=list:longest,full  " Command <Tab> completion
set winminheight=0              " Windows can be 0 line high

"
" Text formatting defaults. These are generally sensible and should comply
" with Python PEP8.
set autoindent                  " Indent at the same level of the previous line
set copyindent                  " use tabs or spaces as on prior line
set nosmartindent               " Avoid snotfights with syntax plugins (i hope)
set shiftwidth=4                " Use indents of four spaces
set expandtab                   " i prefer the go tab for indent but meh
set tabstop=4                   " An indentation every four columns
set softtabstop=4               " Let backspace delete indent
set textwidth=79                " card images will never die!
set nojoinspaces                " Prevents inserting two spaces on join line 

"
" TODO: ripgrep?
set grepprg=LC_ALL=C\ grep\ -nrsh

"
" TODO: is there a better way to access headers like <sys/types.h>
" for jumping to files? right now i'm hardcoding to my lubuntu system.
"set path+=/usr/include/x86_64-linux-gnu
set path+=**                    " allow :e file autocomplete in subdirectories

"
" tweak netrw to get a nerdtree-lite
" from https://shapeshed.com/vim-netrw/
let g:netrw_banner = 0
let g:netrw_liststyle = 3
let g:netrw_browse_split = 4
let g:netrw_altv = 1
let g:netrw_winsize = 25

" ----------------------------------------------------------------------------
" Key remaps
" ----------------------------------------------------------------------------

" ----------------------------------------------------------------------------
" my preferred leaders...
" be sure to unmap space, you'll see lags in insert mode otherwise
" ----------------------------------------------------------------------------
nnoremap <space> <nop>
let mapleader = " "
let maplocalleader="\\"

" ----------------------------------------------------------------------------
" from vimtips wiki, syntax highlighting group under cursor
" ----------------------------------------------------------------------------
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . "> trans<" . synIDattr(synID(line("."),col("."),0),"name") . "> lo<" . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" ----------------------------------------------------------------------------
" Quickfix
" ----------------------------------------------------------------------------
nnoremap ]q :cnext<cr>zz
nnoremap [q :cprev<cr>zz
nnoremap ]l :lnext<cr>zz
nnoremap [l :lprev<cr>zz

" ----------------------------------------------------------------------------
" Buffers
" ----------------------------------------------------------------------------
nnoremap ]b :bnext<cr>
nnoremap [b :bprev<cr>

" ----------------------------------------------------------------------------
" Tabs
" ----------------------------------------------------------------------------
nnoremap ]t :tabn<cr>
nnoremap [t :tabp<cr>

" ----------------------------------------------------------------------------
" <tab> / <s-tab> | Circular windows navigation
" ----------------------------------------------------------------------------
nnoremap <tab>   <c-w>w
nnoremap <S-tab> <c-w>W

" ----------------------------------------------------------------------------
" ctags
" ----------------------------------------------------------------------------
map <leader>t :silent !ctags -R<CR><C_L>
" TODO: the following is c specific, need to consider generalization
map <leader>/ :execute "vimgrep // **/*[.ch]"

" ----------------------------------------------------------------------------
" abbreviations for typos  and common
" text.
" ----------------------------------------------------------------------------
iabbrev @@ blametroi@gmail.com
iabbrev ccopy Copyright 2022 Troy Brumley, all rights reserved.
iabbrev ssig -- <cr>Troy Brumley<cr>blametroi@gmail.com
" u+29D0 ⧐
" TODO: add some common unicode glyps

" TODO: these belong in plugin specific files (after/ and such)
" ----------------------------------------------------------------------------
" mappings and customization for specific filetypes
" ----------------------------------------------------------------------------
augroup filetype_basic
  autocmd!
  autocmd filetype basic nnoremap <buffer> <localleader>c I'<esc>
  autocmd BufNewFile,BufRead *.bas set ft=basic
  autocmd BufNewFile,BufRead *.bi  set ft=basic
  autocmd BufNewFile,BufRead *.bm  set ft=basic
  autocmd BufNewFile,BufRead *.bas compiler fbc
augroup END

augroup filetype_scheme
    autocmd!
    autocmd filetype scheme let b:is_chicken=1
    autocmd BufNewFile,BufRead *.scm set ft=scheme
    autocmd BufNewFile,BufRead *.sld set ft=scheme
    autocmd BufNewFile,BufRead *.rkt set ft=scheme
    autocmd BufNewFile,BufRead *.ss  set ft=scheme
augroup END

" testing
augroup filetype_pascal
  autocmd!
  autocmd filetype pascal nnoremap <buffer> <localleader>c I//<esc>
augroup END

augroup filetype_c
  autocmd!
  autocmd filetype c nnoremap <buffer> <localleader>c I//<esc>
augroup END    

augroup filetype_vim
  autocmd!
  autocmd FileType vim setlocal foldmethod=marker
augroup END

augroup filetype_cobol
  autocmd!
  autocmd filetype cobol filetype indent off
augroup END
