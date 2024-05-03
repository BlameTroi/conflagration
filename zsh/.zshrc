# .zshrc
#
# sourced only for interactive shell invocations. Here's where
# we add the eye candy and zsh command line stuff.

# completions for exercism
typeset -U fpath
#export fpath=(~/.zsh/functions $fpath)

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# emacsish
bindkey -e
[[ $EMACS = t ]] && unsetopt zle

# completion
autoload -Uz compinit
compinit

# prompts with some vcs goodness
# ref zsh docs and https://salferrarello.com/zsh-git-status-prompt/
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
PS1='%n %~$vcs_info_msg_0_ $ '

# Shell Options ...
#
# Changing Directories
setopt AUTO_CD
setopt AUTO_PUSHD
setopt CDABLE_VARS
setopt PUSHD_TO_HOME
#
# Completion
setopt ALWAYS_TO_END
setopt AUTO_LIST
setopt HASH_LIST_ALL
#
# Expansion and Globbing
setopt MARK_DIRS
#
# History
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
#
# Input/Output
setopt ALIASES
setopt IGNORE_EOF
setopt INTERACTIVE_COMMENTS
setopt HASH_CMDS
setopt HASH_DIRS
#
# Job Control
setopt CHECK_JOBS
setopt CHECK_RUNNING_JOBS
setopt NOTIFY
#
# Prompting
setopt PROMPT_SUBST
setopt PROMPT_BANG
setopt PROMPT_SP
setopt PROMPT_PERCENT
#
# Scripts and Functions
setopt C_BASES
#
# aliases

# .zshalias
#
# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# example of single item needed after test
# type ag >/dev/null 2>&1 && alias grep=ag
# for multiples
# if --above to &&--; then
#   blah
# fi

# nvim if available
if type nvim >/dev/null 2>&1; then
  alias vim='nvim'
  alias vimdiff='nvim -d'
fi

# mc doesn't look good in color
alias mc='mc --nocolor'

# Default to human readable figures
alias df='df -h'
alias du='du -h'
#
# editing
# alias e='emacsclient -t'
#alias emacsclient='emacsclient -t'
#alias killemacs='emacsclient -e "(save-buffers-kill-emacs)"'
#
# Misc
alias less='less -r'                          # raw control characters
#alias whence='type -a'                        # where, of a sort
#
# Some shortcuts for different directory listings
alias ls='ls -F' # --color=auto'
alias l='ls -F' # --color=auto'
alias ll='ls -lF' # --color=auto'      # long list
alias la='ls -AF' # --color=auto'      # almost all (excludes . and ..)
alias l='ls -CF' # --color=auto'       # list by columns, suffix type indicator
alias lla='ls -lAF' # --color=auto'    # long list almost all
#
# longtime basic programmer
alias cls='clear'
#
# fzf should use ripgrep if available
if type rg &> /dev/null 2>&1 ; then
  export FZF_DEFAULT_COMMAND='rg --files'
  export FZF_DEFAULT_OPTS='-m --color=bw'
fi
#
# Umask
#
# /etc/profile sets 022, removing write perms to group + others.
# Set a more restrictive umask: i.e. no exec perms for others:
# umask 027
# Paranoid: neither group nor others have any perms:
# umask 077
umask 033
#
# Path additions
#
# lifted from /etc/profile for my .bashrc
#prependpath () {
#    case ":$PATH:" in
#        *:"$1":*)
#            ;;
#        *)
#            PATH="$1:$PATH"
#    esac
#}
#mb=$HOME/.gem/ruby/2.5.0/bin
#[[ -d $mb ]] && prependpath "$mb"
#mb=$HOME/bin
#[[ -d $mb ]] && prependpath "$mb"
#unset mb
#unset prependpath
# fzf integration
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

#export FZF_DEFAULT_COMMAND='fd --type file'
#export FZF_DEFAULT_COMMAND='fd --type file --follow --hidden --exclude .git --color=always'
#export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
#export FZF_DEFAULT_OPTS='--color=fg:#f8f8f2,bg:#282a36,hl:#bd93f9 --color=fg+:#f8f8f2,bg+:#44475a,hl+:#bd93f9 --color=info:#ffb86c,prompt:#50fa7b,pointer:#ff79c6 --color=marker:#ff79c6,spinner:#ffb86c,header:#6272a4'

# use nvim as man pager if it is available
type nvim >/dev/null 2>&1 && export MANPAGER='nvim +Man!'

export PATH="/opt/homebrew/opt/ruby/bin:$PATH"
export PATH=/usr/local/smlnj/bin:"$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/.local/bin/smlformat:$PATH"

