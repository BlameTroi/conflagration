# .zshenv

# sourced on all shell invocations. generally things that aren't
# needed for an interactive shell, which belong in .zshrc.


# standard proper locations. use these!

export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_STATE_HOME="$HOME/.local/state"


# for zsh, which i'm not currently using:
#
# export ZSH="$HOME/.oh-my-zsh"
# export ZSH_CUSTOM="$HOME/.zshcustom"


# on ubuntu and macos, do not use manpath variable! see manpath
# command and /etc/manpath.config.
#
# typeset -U MANPATH
# export manpath=(~/.local/share/man /usr/local/man $manpath[@])


# my path extensions. i don't have anything that uses .cargo and go is
# managed by homebrew, so only my personal bin needs to be included.

typeset -U PATH
export path=(~/.local/bin $path[@])
# export path=(~/.local/bin ~/.cargo/bin /usr/local/go/bin $path[@])


# umask for universal read, owner read write and execute: symbolically
# u=rwx,go=r. umask turns -off- permissions, so 033 turns off the
# write (2) and execute (1) bits.

umask 033


# all my editing is emacs these days. this may need some tweaking
# to work correctly on macos.

export VISUAL="emacs"
export EDITOR=$VISUAL
export ALTERNATE_EDITOR=""


# Timezone, set TZ$ to /etc/timezone if it exists, or default to
# America/New_York.
if [ ! -f /etc/timezone ]; then
    export TZ="America/New_York"
else
    export TZ=$(</etc/timezone)
fi


# guile paths

export GUILE_LOAD_PATH="/opt/homebrew/share/guile/site/3.0"
export GUILE_LOAD_COMPILED_PATH="/opt/homebrew/lib/guile/3.0/site-ccache"
export GUILE_SYSTEM_EXTENSIONS_PATH="/opt/homebrew/lib/guile/3.0/extensions"


# here i was exporting to CPATH and LIBRARY_PATH, but the right
# way to do this is in the compilation steps controlled by
# make or cmake.


# on macos since sequoia it seems that libmalloc is using a new zone
# and that leads to warnings about failure to allocate or reserve space
# in the new 'nano' zone.
#
# those are just warnings, it isn't an error to not use the nano zone.
#
# this disables use of the nano zone which quiets those messages. this
# is only in my terminal and shell sessions, not the system as a whole.

export MallocNanoZone=0


# i've given in to the pressure to switch to cmake. i default my
# builds to use ninja instead of gnu make because multiple
# configurations are built in.

export CMAKE_GENERATOR="Ninja Multi-Config"


# how did i not know about this? unprefixed paths on cd are
# checked to see if they are immediate descendants of
# directories specified here.

typeset -U CDPATH
export cdpath=(. ~/projects ~ $cdpath[@])
#export CDPATH=.:~/projects:~


# not enough programs honor this, but whre possible i prefer to
# turn off colored output.

export NO_COLOR=1
