# .zshrc

# sourced only for interactive shell invocations. Here's where
# we add the eye candy and zsh command line stuff.


# a speedup for git and others, mark as dirty only files that
# git cares about.

DISABLE_UNTRACKED_FILES_DIRTY="true"


# emacsish

bindkey -e
[[ $EMACS = t ]] && unsetopt zle


# completion

autoload -Uz compinit
compinit


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
PS1='%n %~$vcs_info_msg_0_ $ '


# shell options ...


# changing Directories

setopt AUTO_CD
setopt AUTO_PUSHD
setopt CDABLE_VARS
setopt PUSHD_TO_HOME


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


# default to human readable figures

alias df='df -h'
alias du='du -h'


# editing. i need to get emacsclient working reliably on  macos.

# alias e='emacsclient -t'
# alias emacsclient='emacsclient -t'
# alias killemacs='emacsclient -e "(save-buffers-kill-emacs)"'


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

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
#export FZF_DEFAULT_COMMAND='fd --type file'
#export FZF_DEFAULT_COMMAND='fd --type file --follow --hidden --exclude .git --color=always'
#export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
#export FZF_DEFAULT_OPTS='--color=fg:#f8f8f2,bg:#282a36,hl:#bd93f9 --color=fg+:#f8f8f2,bg+:#44475a,hl+:#bd93f9 --color=info:#ffb86c,prompt:#50fa7b,pointer:#ff79c6 --color=marker:#ff79c6,spinner:#ffb86c,header:#6272a4'


# Path additions -- odd path extensions from homebrew and similar

export PATH="/opt/homebrew/opt/ruby/bin:$PATH"

# export PATH=/usr/local/smlnj/bin:"$PATH"
# export PATH="$HOME/.local/bin/smlformat:$PATH"

export PATH="/opt/homebrew/opt/node@20/bin:$PATH"
# if you need compilers to find node ...
# export LDFLAGS="-L/opt/homebrew/opt/node@20/lib"
# export CPPFLAGS="-I/opt/homebrew/opt/node@20/include"
