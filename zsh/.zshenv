# .zshenv

# sourced on all shell invocations. generally things that aren't
# needed for an interactive shell, which belong in .zshrc.


# standard proper locations. use these!

export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_STATE_HOME="$HOME/.local/state"

# from brew shellenv zsh

export HOMEBREW_PREFIX="/opt/homebrew";
export HOMEBREW_CELLAR="/opt/homebrew/Cellar";
export HOMEBREW_REPOSITORY="/opt/homebrew";

fpath[1,0]="/opt/homebrew/share/zsh/site-functions";
PATH="/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin:/Library/Apple/usr/bin:/usr/local/share/dotnet:~/.dotnet/tools:/opt/homebrew/opt/tcl-tk@8/bin:/opt/homebrew/opt/node@20/bin:/Users/troi/.local/bin/smlformat:/usr/local/smlnj/bin:/opt/homebrew/opt/ruby/bin:/Users/troi/.local/bin"; export PATH;

[ -z "${MANPATH-}" ] || export MANPATH=":${MANPATH#:}";
export INFOPATH="/opt/homebrew/share/info:${INFOPATH:-}";

# on ubuntu and macos, do not use manpath variable! see manpath
# command and /etc/manpath.config.
#
# typeset -U MANPATH
# export manpath=(~/.local/share/man /usr/local/man $manpath[@])


# my path extensions. i don't have anything that uses .cargo and go is
# managed by homebrew, so only my personal bin needs to be included.
# 
# by the time the path hits .zshrc, a bunch of stuff has been put in
# front of my .local/bin, leaving this here but it's redundant in 
# login shells.
typeset -U PATH
export path=(~/.local/bin $path[@])
# export path=(~/.local/bin ~/.cargo/bin /usr/local/go/bin $path[@])


# umask for universal read, owner read write and execute: symbolically
# u=rwx,go=r. umask turns -off- permissions, so 033 turns off the
# write (2) and execute (1) bits.

umask 033

# Timezone, set TZ$ to /etc/timezone if it exists, or default to
# America/New_York.
if [ ! -f /etc/timezone ]; then
    export TZ="America/New_York"
else
    export TZ=$(</etc/timezone)
fi

# These are set by apple terminal and iterm2, but it looks as if every
# app does something different. As 'pipenv' wants LANG set, I hard code
# these here. LC_ALL=C ensures that sort order is byte, not character.
# Set it to en_US.UTF-8 if you want to sort non ascii text.

export LANG="en_US.UTF-8"
export LC_COLLATE="en_US.UTF-8"
export LC_CTYPE="en_US.UTF-8"
export LC_MESSAGES="en_US.UTF-8"
export LC_MONETARY="en_US.UTF-8"
export LC_NUMERIC="en_US.UTF-8"
export LC_TIME="en_US.UTF-8"
export LC_ALL="C"

# Guile paths

export GUILE_LOAD_PATH="/opt/homebrew/share/guile/site/3.0"
export GUILE_LOAD_COMPILED_PATH="/opt/homebrew/lib/guile/3.0/site-ccache"
export GUILE_SYSTEM_EXTENSIONS_PATH="/opt/homebrew/lib/guile/3.0/extensions"


# On Macos since Sequoia it seems that libmalloc is using a new zone
# and that leads to warnings about failure to allocate or reserve space
# in the new 'nano' zone.
#
# Those are just warnings, it isn't an error to not use the 'nano' zone.
#
# This disables use of the nano zone which quiets those messages. This
# is only in my terminal and shell sessions, not the system as a whole.

export MallocNanoZone=0


# i've given in to the pressure to switch to cmake. i default my
# builds to use ninja instead of gnu make because multiple
# configurations are built in.

export CMAKE_GENERATOR="Ninja Multi-Config"


# ### dotnet ###
#export DOTNET_ROOT = ~/.dotnet

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
export LDFLAGS="-L/opt/homebrew/opt/node@20/lib $LDFLAGS"
export CPPFLAGS="-I/opt/homebrew/opt/node@20/include $CPPFLAGS"


# `readline' is needed for Chicken Ccheme's `breadline' egg.
# For compilers to find readline you may need to set:
export LDFLAGS="-L/opt/homebrew/Cellar/readline/8.2.13/lib $LDFLAGS"
export CPPFLAGS="-I/opt/homebrew/Cellar/readline/8.2.13/include $CPPFLAGS"

# For pkg-config to find readline you may need to set:
export PKG_CONFIG_PATH="/opt/homebrew/Cellar/readline/8.2.13/lib/pkgconfig"

# i've given in to the pressure to switch to cmake. i default my
# builds to use ninja instead of gnu make because multiple
# configurations are built in.

export CMAKE_GENERATOR="Ninja Multi-Config"


# Pyenv depends on pkgconfig which depends up tcl-tk@8, but the current release
# is @9. Fudge up the path. it might be that pkgconfig path is all we need, but
# let's do it all. If you need to have tcl-tk@8 first in your PATH, run:

export PATH="/opt/homebrew/opt/tcl-tk@8/bin:$PATH"

# For compilers to find tcl-tk@8 you may need to set:

export LDFLAGS="-L/opt/homebrew/opt/tcl-tk@8/lib $LDFLAGS"
export CPPFLAGS="-I/opt/homebrew/opt/tcl-tk@8/include $CPPFLAGS"

# For pkg-config to find tcl-tk@8 you may need to set:

export PKG_CONFIG_PATH="/opt/homebrew/opt/tcl-tk@8/lib/pkg $PKG_CONFIG_PATH"

# Currently LDFLAGS and CPPFLAGS include readline, node, and tcl-tk, in
# reverse order. It is as yet unclear if I should delete or move this
# strictly to (C)Make scripts, but I know pyenv wants tcl-tk@8.

# added by pyenv
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init - zsh)"

# virtual environment plugin
eval "$(pyenv virtualenv-init -)"
