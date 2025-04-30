# .zshrc

# This is sourced for interactive shell invocations.

# A speedup for Git and others, mark as dirty only files that
# Git cares about.

DISABLE_UNTRACKED_FILES_DIRTY="true"

# Completion:

autoload -Uz compinit
compinit

# For some reason while I use Vim elsewhere, the Emacs bindings work
# better for me on the command line.

bindkey -e

# Add VCS status information to prompts:

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

# Shell options:

# Hex and Octal output as I like it:

setopt C_BASES
setopt OCTAL_ZEROES

# Changing Directories:

setopt AUTO_CD
setopt AUTO_PUSHD
setopt PUSHD_TO_HOME
setopt CHASE_LINKS
setopt PUSHD_IGNORE_DUPS
DIRSTACKSIZE=8
setopt PUSHD_MINUS 
alias dh='dirs -v'

# Completion:

setopt ALWAYS_TO_END
setopt AUTO_LIST
setopt HASH_LIST_ALL

# Expansion and globbing:

setopt MARK_DIRS

# History:

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

# Miscellaneous:

setopt ALIASES
setopt IGNORE_EOF
setopt INTERACTIVE_COMMENTS
setopt HASH_CMDS
setopt HASH_DIRS

# Job control:

setopt CHECK_JOBS
setopt CHECK_RUNNING_JOBS
setopt NOTIFY

# Prompting:

setopt PROMPT_SUBST
setopt PROMPT_BANG
setopt PROMPT_SP
setopt PROMPT_PERCENT

# Scripts and functions

# none at the moment:

# Prefer Neovim if available:

export VISUAL="vim"
export EDITOR="vim"
export ALTERNATE_EDITOR=""

if type nvim >/dev/null 2>&1; then
	alias vim='nvim'
	alias vimdiff='nvim -d'
	export VISUAL="nvim"
	export EDITOR="nvim"
	export MANPAGER='nvim +Man!'
fi

# Human readable figures:

alias df='df -h'
alias du='du -h'

# Don't shoot your toes off:

alias rm="echo use path to rm if you are sure about this! or try trash"

# Miscellaneous commands:

alias less='less -r'                # raw control characters
alias ls='ls -F --color=auto'
alias l='ls -F --color=auto'
alias ll='ls -lF --color=auto'      # long list
alias la='ls -AF --color=auto'      # almost all (excludes . and ..)
alias l='ls -CF --color=auto'       # list by columns, suffix type indicator
alias lla='ls -lAF --color=auto'    # long list almost all

# I am a longtime Basic programmer:

alias cls='clear'

# fzf should use ripgrep if available

if type rg &> /dev/null 2>&1 ; then
	export FZF_DEFAULT_COMMAND='rg --files'
	export FZF_DEFAULT_OPTS='-m --color=bw'
fi


# fzf integration

if type fzf >/dev/null 2>&1; then
	source <(fzf --zsh)
fi

#[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
#export FZF_DEFAULT_COMMAND='fd --type file'
#export FZF_DEFAULT_COMMAND='fd --type file --follow --hidden --exclude .git --color=always'
#export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
#export FZF_DEFAULT_OPTS='--color=fg:#f8f8f2,bg:#282a36,hl:#bd93f9 --color=fg+:#f8f8f2,bg+:#44475a,hl+:#bd93f9 --color=info:#ffb86c,prompt:#50fa7b,pointer:#ff79c6 --color=marker:#ff79c6,spinner:#ffb86c,header:#6272a4'

# Tweaking around nvim. My old configuration is in .config/nvim, which
# is the default. It can be overridden by setting NVIM_APPNAME to the
# directory holding the desired config.

export NVIM_APPNAME=lazyvim
echo neovim is $NVIM_APPNAME
