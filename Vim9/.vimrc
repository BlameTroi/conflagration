" .vimrc
" vim: tw=79:sw=3:ts=3:et:fdm=marker

set nocompatible        " Must be first
set encoding=utf-8

" ------------------------------------------------------------------------------
" Space is a fine leader, but be sure to unmap space to avoid delays on some
" systems.

let mapleader = " "
let maplocalleader=","
nnoremap <space> <leader>

" ------------------------------------------------------------------------------
" Terminal configuration:
"
" I mostly use kitty, but sometimes iTerm2. All these &t_ settings should be
" done *before* :colorscheme. Also do not change term= after this.
"
" Per the kitty faq, these changes should improve Vim's recognition of any
" modern terminal, so this is unguarded.

" Mouse support -- no mouse
set mouse=
"set mouse=a
"set ttymouse=sgr
"set balloonevalterm
" Styled and colored underline support
let &t_AU = "\e[58:5:%dm"
let &t_8u = "\e[58:2:%lu:%lu:%lum"
let &t_Us = "\e[4:2m"
let &t_Cs = "\e[4:3m"
let &t_ds = "\e[4:4m"
let &t_Ds = "\e[4:5m"
let &t_Ce = "\e[4:0m"
" Strikethrough
let &t_Ts = "\e[9m"
let &t_Te = "\e[29m"
" Truecolor support
let &t_8f = "\e[38:2:%lu:%lu:%lum"
let &t_8b = "\e[48:2:%lu:%lu:%lum"
let &t_RF = "\e]10;?\e\\"
let &t_RB = "\e]11;?\e\\"
" Bracketed paste
let &t_BE = "\e[?2004h"
let &t_BD = "\e[?2004l"
let &t_PS = "\e[200~"
let &t_PE = "\e[201~"
" Cursor control
let &t_RC = "\e[?12$p"
let &t_SH = "\e[%d q"
let &t_RS = "\eP$q q\e\\"
let &t_VS = "\e[?12l"
" Set cursor shape by mode: n=block, i=bar, r=underbar.
"  1 -> blinking block
"  2 -> solid block 
"  3 -> blinking underscore
"  4 -> solid underscore
"  5 -> blinking vertical bar
"  6 -> solid vertical bar
let &t_SI.="\e[5 q" "SI = INSERT mode
let &t_SR.="\e[4 q" "SR = REPLACE mode
let &t_EI.="\e[1 q" "EI = NORMAL mode (ELSE)
" Focus tracking
let &t_fe = "\e[?1004h"
let &t_fd = "\e[?1004l"
execute "set <FocusGained>=\<Esc>[I"
execute "set <FocusLost>=\<Esc>[O"
" Window title
let &t_ST = "\e[22;2t"
let &t_RT = "\e[23;2t"
" vim hardcodes background color erase even if the terminfo file does
" not contain bce. This causes incorrect background rendering when
" using a color theme with a background color in terminals such as
" kitty that do not support background color erase.
let &t_ut=''

" -----------------------------------------------------------------------------
" Optional Vim built-ins.

packadd! matchit                  " allows jumping via %
packadd! hlyank                   " highlight yanked text
let g:hlyank_duration = 150

" -----------------------------------------------------------------------------
" Additional plugins.
"
" I'm using vim-plug. You have to have downloaded it manually. The additional
" plugins should land in stdpath('data')/plugged

call plug#begin()

" Install as plugin to get help files ----------------------------------------
Plug 'junegunn/vim-plug'

" UI colors, appearance, and behavior ----------------------------------------
Plug 'tpope/vim-sensible'
Plug 'ryanoasis/vim-devicons'
Plug 'BlameTroi/vim-lucius'
Plug 'vim-scripts/SyntaxAttr.vim'
Plug 'farmergreg/vim-lastplace'
Plug 'KaraMCC/vim-streamline'
let g:streamline_enable_devicons = 1
let g:streamline_minimal_ui      = 0
Plug 'ciaranm/securemodelines'
let g:secure_modelines_verbose = 1
let g:secure_modelines_leave_modeline = 1
Plug 'airblade/vim-gitgutter'
let g:gitgutter_set_sign_backgrounds = 1

" Editing and motion ----------------------------------------------------------
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'girishji/easyjump.vim'
" Plug 'monkoose/vim9-stargate'
" Discoverability and help ----------------------------------------------------
" Todo: this isn't quite as good as the lua from folke, or the emacs
" implementation, but it might be useful. I need to fix up all the colors
" though.
"
" a simple black and white would suffice.
"
" WhichKey
" Plug 'liuchengxu/vim-which-key'
" let g:which_key_use_floating_win = 1
" let g:which_key_floating_relative_win = 1
" hi WhichKeyTrigger ctermfg=232 ctermbg=100 guifg=#333300 guibg=#898989
" hi WhichKeyName cterm=bold ctermfg=171 ctermbg=239 gui=bold guifg=#d75fd7 guibg=#4e4e4e
" To register the descriptions when using the on-demand load feature,
" use the autocmd hook to call which_key#register(), e.g., register for the Space key:
" autocmd! User vim-which-key call which_key#register('<Space>', 'g:which_key_map'):1
"hi WhichKeyDesc guifg=#ffcb6b  ctermfg=178 guibg=NONE ctermbg=NONE
"hi WhichKey  guifg=#89ddff ctermfg=74
"nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
"nnoremap <silent> <localleader> :<c-u>WhichKey  ','<CR>

" Assorted completions --------------------------------------------------------
Plug 'girishji/vimcomplete'
Plug 'girishji/vimsuggest'
Plug 'chrisbra/unicode.vim'

" Fuzzy finder options, fuzzyy is vim9script with rg support ------------------
Plug 'Donaldttt/fuzzyy'
" The following were in old configs but I never really used them, looking to
" fuzzyy above but keeping these for notes.
" " this or cntrlp?
" Plug 'junegunn/fzf', {'do': './install --all'}
" Plug 'junegunn/fzf.vim'
" " this or fzf?
" Plug 'ctrlpvim/ctrlp.vim'
" let g:ctrlp_custom_ignore = '\.git\|node_modules\|\.cache'

" We'll never lose the tree view for file systems -----------------------------
Plug 'preservim/nerdtree', { 'on': 'NERDTreeToggle' }

" Directory level diff with drill down ----------------------------------------
Plug 'ZSaberLv0/ZFVimBackup', {'on': 'ZFVimBackup'}
Plug 'ZSaberLv0/ZFVimIgnore', {'on': 'ZFDirDiff'}
Plug 'ZSaberLv0/ZFVimJob', {'on': 'ZFDirDiff'}
Plug 'zsaberlv0/zfvimdirdiff', {'on': 'ZFDirDiff'}

" A color scheme template/designer --------------------------------------------
Plug 'lifepillar/vim-colortemplate'

" Assorted languages and filetypes -------------------------------------------
Plug '~/Projects/Neovim/qb64dev.nvim/'
Plug '~/Projects/Neovim/qbcolor.vim/'
let filetype_bas = "qb64"

" My WIP PureBasic support ---------------------------------------------------
Plug '~/Projects/Neovim/PureBasic-ftplugin-vim9/'

call plug#end()

" ------------------------------------------------------------------------------
" Coloring and highlighting

set background=dark
set termguicolors
colorscheme lucius
LuciusBlackHighContrast
highlight Comment gui=italic cterm=italic
highlight! link SignColumn LineNR

" ------------------------------------------------------------------------------
" These are carried over from my Neovim configuration. I'm not a fan of editor
" config.

let g:editorconfig = 0
let g:EditorConfig_disable = 1

" ------------------------------------------------------------------------------
" UI appearance and some behavior
" Text formatting defaults. The default setting is that of a slightly less dumb
" terminal. Per filetype configuration can be done with modelines and filetype
" plugins.

set autowrite                   " So i don't forget.
set backspace=indent,eol,start  " Backspace for dummies
set nofoldenable                " Disallow code folding.
set foldmethod=manual           " Use indenting for folds if turned on.
set cmdheight=1                 " More space in command line.
set foldlevelstart=99           " And open them when loading a file.
set hidden                      "
set hlsearch                    " Highlight search terms.
set ignorecase                  " Case insensitive search.
set noincsearch                 " Don't chase the searches.
set laststatus=2                " Always show status lines.
set linespace=0                 " No extra spaces between rows.
set cursorline                  " Highlight current line.
set noshowmode                  " Let the status line displays mode
set number                      " Line numbers on.
set numberwidth=6               " xedit legacy
set norelativenumber            " Eww!
set ruler                       " Show the ruler.
set scrolljump=1                " Lines to scroll when cursor leaves screen
set scrolloff=3                 " Minimum lines to keep above and below cursor
set showcmd                     " Show partial commands in status line and
set showmatch                   " Show matching brackets/parenthesis
set sidescroll=8                " Scroll in chunks.
set smartcase                   " Case sensitive when uc present.
set splitbelow                  " How I like splits.
set splitright                  " How I like splits.
set tabpagemax=5                " Only show 5 tabs
set whichwrap=b,s,h,l,<,>,[,]   " Backspace and cursor keys wrap too.
set wildmenu                    " Show list instead of just completing.
set wildmode=list:longest,full  " Command <Tab> completion.
set winminheight=0              " Windows can be 0 line high. (NOTE: Why)
set infercase                   " During completion.
" set isexpand=???              " Not sure I need this, default is ''.
set autoindent                  " Indent at the same level of the previous line
set copyindent                  " use tabs or spaces as on prior line
set nosmartindent               " Avoid snotfights with syntax plugins (i hope)
set shiftwidth=8                " Default settings should work like a teletype.
set noexpandtab                 " ..
set tabstop=8                   " ..
set softtabstop=8               " ..
set textwidth=79                " card images will never die!
set nojoinspaces                " Prevents inserting two spaces on join line 
set nowrap                      " Don't wrap long lines.
set breakindent                 " Wrapped line indent matches prior line.
set linebreak                   " Wrap at boundaries other than column number.
set indentexpr=                 " make sure it's empty
set formatoptions=jcroqlnt      " Kitchen sink formatting.
" set breakat= ???              " Do I want to tweak this?
set showbreak=""                " No margin marker on wrap.
set list                        " Display invisibles.
set listchars=tab:»\ ,trail:·,nbsp:␣
set clipboard=unnamed           " Integrate with system clipboard. (NOTE: not working well)
set confirm                     " Are you absolutely sure about that?
set grepprg=LC_ALL=C\ grep\ -nrsh " I'm not sure about this setting.
" TODO: ripgrep?
set path+=**                    " Directories to search for find and such.
set shortmess-=S                " Display (x/y) for search hits
set shortmess+=I                " No vim info splash screen

set spelllang=en_us
set spellsuggest=best,10
set spellfile=~/Notepad/spell/en.utf-8.add
set thesaurus=~/Notepad/spell/thesaurus.txt
" does not exist in vim? set inccommand = "split"
set confirm

" ----------------------------------------------------------------------------
" Key maps


" Display hightlight information for text at cursor.
nnoremap <F10> :call SyntaxAttr()<CR>

" Quickfix navigation.
nnoremap ]q :cnext<cr>zz
nnoremap [q :cprev<cr>zz
nnoremap ]l :lnext<cr>zz
nnoremap [l :lprev<cr>zz

" Buffer navigation.
nnoremap ]b :bnext<cr>
nnoremap [b :bprev<cr>

" Disable arrow keys for normal, insert, and visual mode. They are still
" available in command mode.
nnoremap <left> ""
nnoremap <right> ""
nnoremap <up> ""
nnoremap <down> ""
inoremap <left> ""
inoremap <right> ""
inoremap <up> ""
inoremap <down> ""
vnoremap <left> ""
vnoremap <right> ""
vnoremap <up> ""
vnoremap <down> ""

" Use <A-hjkl> for shifting lines in a buffer.
nnoremap <A-j> <cmd>execute 'move .+' . v:count1<cr>=="
nnoremap <A-k> <cmd>execute 'move .-' . (v:count1 + 1)<cr>=="
inoremap <A-j> <esc><cmd>m .+1<cr>==gi"
inoremap <A-k> <esc><cmd>m .-2<cr>==gi"

" Do the right thing with j/k for wrapped text. -----------------------------
noremap <silent> k gk
noremap <silent> j gj
noremap <silent> 0 g0
noremap <silent> $ g$

" Create a split.
nnoremap <leader>- <C-w>s
nnoremap <leader>\| <C-w>v
nnoremap <leader>wd <C-w>c

" Move around splits.
nnoremap <C-h> <C-w><C-h>
nnoremap <C-l> <C-w><C-l>
nnoremap <C-j> <C-w><C-j>
nnoremap <C-k> <C-w><C-k>

" Resize splits.
nnoremap <C-Left> <Cmd>vertical resize -1<CR>
nnoremap <C-Down> <Cmd>resize -1<CR>
nnoremap <C-Up> <Cmd>resize +1<CR>
nnoremap <C-Right> <Cmd>vertical resize +1<CR>

" Turn off the lights.
nnoremap <Esc> "<cmd>nohlsearch<CR>"

" Terminal helpers.
tnoremap <C-/> <cmd>close<cr>
tnoremap <Esc><Esc> <C-\><C-n>

" ----------------------------------------------------------------------------
" Abbreviations for typos and common text.
iabbrev @@ blametroi@gmail.com
iabbrev ccopy Copyright 2022 Troy Brumley, all rights reserved.
iabbrev ssig -- <cr>Troy Brumley<cr>blametroi@gmail.com
iabbrev ffooter --
         \<CR>[BlameTroi](BlameTroi@Gmail.com)<SPACE><SPACE>
         \<CR>is Troy Brumley..
         \<CR>
         \<CR>So let it be written...<SPACE><SPACE>
         \<CR>...so let it be done.<CR>

" pending

" after/ftplugin/markdown.vim
" vim: ts=3 sw=3 sts=3 et ai fdm=manual
" My markdown preferences.

augroup markdown_group
   autocmd!
   autocmd Filetype markdown set wrap tw=0 formatoptions=qt linebreak nonumber spell
augroup END

" --- ~/.config/kickstart/lua/config/autocmds/qb64.lua

" -- Basic is QB64
" local qb64group = vim.api.nvim_create_augroup("qb64", {
"   clear = true,
" })

" vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
"   desc = "Basic becomes QB64",
"   group = qb64group,
"   pattern = { "*.bas", "*.bi", "*.bm", "*.BAS", "*.BI", "*.BM" },
"   callback = function ()
"     vim.cmd([[
"  let g:qb64dev_qb64_directory = "/Users/troi/Projects/Basic/qb64pe/"
"  let g:qb64dev_autofind_qb64 = 0
"  nnoremap <F5> :call qb64dev#QB64CompileAndRun()<cr>
"  nnoremap <F11> :call qb64dev#QB64Compile()<cr>
"  ]])
"   end,
" })
