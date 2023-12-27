export NO_COLOR=1
# .zshenv
#
# sourced on all shell invocations. generally things that
# aren't needed for an interactive shell, which belong in
# .zshrc.
#
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_STATE_HOME="$HOME/.local/state"
#
# Path to your oh-my-zsh installation.
#export ZSH="$HOME/.oh-my-zsh"
#
# Would you like to use another custom folder than $ZSH/custom?
#export ZSH_CUSTOM="$HOME/.zshcustom"
#
# on ubuntu, do not use manpath variable! see manpath command
# and /etc/manpath.config
#typeset -U MANPATH
#export manpath=(~/.local/share/man /usr/local/man $manpath[@])
#
# PATH="$PATH"
typeset -U PATH
# export path=(~/.local/bin ~/.cargo/bin /usr/local/go/bin $path[@])
export path=(~/.local/bin $path[@])
#
#
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
# export VISUAL="nvim"
# export EDITOR=$VISUAL
# export ALTERNATE_EDITOR=""
#
# Timezone, set TZ$ to /etc/timezone if it exists, or default
# to America/New_York.
if [ ! -f /etc/timezone ]; then
    export TZ="America/New_York"
else
    export TZ=$(</etc/timezone)
fi
#
export GUILE_LOAD_PATH="/opt/homebrew/share/guile/site/3.0"
export GUILE_LOAD_COMPILED_PATH="/opt/homebrew/lib/guile/3.0/site-ccache"
export GUILE_SYSTEM_EXTENSIONS_PATH="/opt/homebrew/lib/guile/3.0/extensions"
