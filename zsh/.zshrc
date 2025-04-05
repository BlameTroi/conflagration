# .zshrc

# sourced only for interactive shell invocations. Here's where
# we add the eye candy and zsh command line stuff.


# a speedup for git and others, mark as dirty only files that
# git cares about.

DISABLE_UNTRACKED_FILES_DIRTY="true"


# Editing

export VISUAL="vim"
export EDITOR="vim"
export ALTERNATE_EDITOR=""

# utility

export MANPAGER='nvim +Man!'

# completion

autoload -Uz compinit
compinit

# readline
bindkey -v

# add some vcs status information to prompts

autoload -Uz add-zsh-hook vcs_info
setopt prompt_subst
add-zsh-hook precmd vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' stagedstr ' +'
zstyle ':vcs_info:*' unstagedstr ' *'
zstyle ':vcs_info:*' formats ' [%b%u%c]'
zstyle ':vcs_info:*' actionformats ' [%b|%a%u%c]'
export PROMPT='%2~ +++ '
export RPROMPT='$vcs_info_msg_0_ [%*]'

# shell options ...

# Hex and Octal output as I like it:
#
setopt C_BASES
setopt OCTAL_ZEROES

# changing Directories

setopt AUTO_CD
setopt AUTO_PUSHD
setopt PUSHD_TO_HOME
setopt CHASE_LINKS
setopt PUSHD_IGNORE_DUPS
DIRSTACKSIZE=8
setopt PUSHD_MINUS 
alias dh='dirs -v'

# completion

setopt ALWAYS_TO_END
setopt AUTO_LIST
setopt HASH_LIST_ALL


# expansion and globbing

setopt MARK_DIRS


# history

setopt APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FCNTL_LOCK
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_SAVE_NO_DUPS
setopt HIST_SAVE_BY_COPY
setopt INC_APPEND_HISTORY_TIME


# input/output

setopt ALIASES
setopt IGNORE_EOF
setopt INTERACTIVE_COMMENTS
setopt HASH_CMDS
setopt HASH_DIRS


# job control

setopt CHECK_JOBS
setopt CHECK_RUNNING_JOBS
setopt NOTIFY


# prompting

setopt PROMPT_SUBST
setopt PROMPT_BANG
setopt PROMPT_SP
setopt PROMPT_PERCENT


# scripts and functions

setopt C_BASES


# aliases

# I removed my separate .zshalias and put the aliases here.

# nvim if available

if type nvim >/dev/null 2>&1; then
	alias vim='nvim'
	alias vimdiff='nvim -d'
	export VISUAL="nvim"
	export EDITOR=""
	export ALTERNATE_EDITOR=""
fi

# default to human readable figures

alias df='df -h'
alias du='du -h'


# editing. i need to get emacsclient working reliably on  macos.

# alias e='emacsclient -t'
# alias emacsclient='emacsclient -t'
# alias killemacs='emacsclient -e "(save-buffers-kill-emacs)"'

# don't shoot your toes off

alias rm="echo use path to rm if you are sure about this! or try trash"


# miscellaneous commands

alias less='less -r'                          # raw control characters
alias ls='ls -F' # --color=auto'
alias l='ls -F' # --color=auto'
alias ll='ls -lF' # --color=auto'      # long list
alias la='ls -AF' # --color=auto'      # almost all (excludes . and ..)
alias l='ls -CF' # --color=auto'       # list by columns, suffix type indicator
alias lla='ls -lAF' # --color=auto'    # long list almost all


# i am a longtime basic programmer

alias cls='clear'


# fzf should use ripgrep if available

if type rg &> /dev/null 2>&1 ; then
	export FZF_DEFAULT_COMMAND='rg --files'
	export FZF_DEFAULT_OPTS='-m --color=bw'
fi


# umask is set in .zshenv




# fzf integration

if type fzf >/dev/null 2>&1; then
	source <(fzf --zsh)
fi

#[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
#export FZF_DEFAULT_COMMAND='fd --type file'
#export FZF_DEFAULT_COMMAND='fd --type file --follow --hidden --exclude .git --color=always'
#export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
#export FZF_DEFAULT_OPTS='--color=fg:#f8f8f2,bg:#282a36,hl:#bd93f9 --color=fg+:#f8f8f2,bg+:#44475a,hl+:#bd93f9 --color=info:#ffb86c,prompt:#50fa7b,pointer:#ff79c6 --color=marker:#ff79c6,spinner:#ffb86c,header:#6272a4'


# Path additions -- odd path extensions from homebrew and similar.
# For some reason my bin, which I set in zshenv, is not in front
# by the time we get here. So ...

export PATH="$HOME/.local/bin:$PATH"

# WHen you don't want Apple's built in Ruby:

export PATH="/opt/homebrew/opt/ruby/bin:$PATH"

# TODO: do i want to insert symlinks for gcc and g++ to the real
# versions in homebrew, or continue to let clang override them?
export PATH=/usr/local/smlnj/bin:"$PATH"
export PATH="$HOME/.local/bin/smlformat:$PATH"

export PATH="/opt/homebrew/opt/node@20/bin:$PATH"
# if you need compilers to find node ...
export LDFLAGS="-L/opt/homebrew/opt/node@20/lib"
export CPPFLAGS="-I/opt/homebrew/opt/node@20/include"

# odin
# export ODIN_ROOT="/opt/homebrew/Cellar/odin/2024-12"
# TODO: is this still needed on Macos?
#export ODIN_ROOT="/opt/homebrew/Cellar/odin/2025-03_1/libexec/"


# `readline' is needed for Chicken Ccheme's `breadline' egg.
# For compilers to find readline you may need to set:
export LDFLAGS="-L/opt/homebrew/Cellar/readline/8.2.13/lib"
export CPPFLAGS="-I/opt/homebrew/Cellar/readline/8.2.13/include"
# For pkg-config to find readline you may need to set:
export PKG_CONFIG_PATH="/opt/homebrew/Cellar/readline/8.2.13/lib/pkgconfig"

# i've given in to the pressure to switch to cmake. i default my
# builds to use ninja instead of gnu make because multiple
# configurations are built in.

export CMAKE_GENERATOR="Ninja Multi-Config"


# how did i not know about this? unprefixed paths on cd are
# checked to see if they are immediate descendants of
# directories specified here.

# it's causing some problems when executable names collide with
# directory names. So off for a bit.
typeset -U CDPATH
#export cdpath=(. $HOME/projects $cdpath[@])
#export CDPATH=.:~/projects:~


# not enough programs honor this, but whre possible i prefer to
# turn off colored output.

export NO_COLOR=1

