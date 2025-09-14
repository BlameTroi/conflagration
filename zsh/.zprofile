# .zprofile

# MacOs started using something called path_helper and it is invoked
# *after* .zshenv. According to this gist:
#
# https://gist.github.com/Linerre/f11ad4a6a934dcf01ee8415c9457e7b2
#
# The order on is:
#
# /etc/zshenv
# ~/.zshenv
# (for login shells)
#   /etc/zprofile     <--- this calls path_helper and undoes .zshenv
#   ~/.zprofile
# (interactive)
#   /etc/zshrc
#   ~/.zshrc
# (login mode)
#   /etc/zlogin
#   ~/.zlogin
#
# I wanted to be a good zsh citizen and use .zshenv, but Apple fucked
# that up. So, my .zshenv are going to .zprofile.
#
# Sigh.
#
# As I really don't use zsh features, I may just roll back to bash. 

# This was the only thing in .zprofile.
eval "$(/opt/homebrew/bin/brew shellenv)"

# Zsh site function extensions:
fpath[1,0]="/opt/homebrew/share/zsh/site-functions";

# standard proper locations. use these!

export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_STATE_HOME="$HOME/.local/state"

# Manpath -- on ubuntu and macos, do not use manpath variable!
# See manpath command and /etc/manpath.config.
#
# [ -z "${MANPATH-}" ] || export MANPATH=":${MANPATH#:}";
# typeset -U MANPATH
# export manpath=(~/.local/share/man /usr/local/man $manpath[@])
#
# But do update infopath.

export INFOPATH="/opt/homebrew/share/info:${INFOPATH:-}";

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
# app does something different. As 'pipenv' wanted LANG set, I hard code
# these here even though I'm not using 'pipenv' at the moment. LC_ALL=C
# ensures that sort order is byte, not character. Set it to en_US.UTF-8
# if you want to sort non ascii text.
#
# I think the best compromise to allow nerd font style glyphs to be
# copy/paste safe from desktop to vim is as below. I want most "internal"
# behavior to follow the C rules. Messages, monitary, numeric, and time
# should follow en_US.

export LANG="en_US.UTF-8"
#export LC_ALL=C
export LC_COLLATE=C
export LC_CTYPE=C
export LC_MESSAGES="en_US.UTF-8"
export LC_MONETARY="en_US.UTF-8"
export LC_NUMERIC="en_US.UTF-8"
export LC_TIME="en_US.UTF-8"

# Guile paths

#export GUILE_LOAD_PATH="/opt/homebrew/share/guile/site/3.0"
#export GUILE_LOAD_COMPILED_PATH="/opt/homebrew/lib/guile/3.0/site-ccache"
#export GUILE_SYSTEM_EXTENSIONS_PATH="/opt/homebrew/lib/guile/3.0/extensions"

# PureBasic paths -- the instructions in Install.txt are "wrong" in
# describing the app bundle structure. I believe that they mean the
# resources directory instead of the top level of the app. The IDE
# executable is in ../MacOS from this.
#
# https://www.purebasic.fr/english/viewtopic.php?p=558520&hilit=command+line+compiler#p558520
#
# The forum post "what is the path of PUREBASIC_HOME" seems to agree with
# this.
export PUREBASIC_HOME="/Applications/PureBasic.app/Contents/Resources"

# On Macos since Sequoia it seems that libmalloc is using a new zone
# and that leads to warnings about failure to allocate or reserve space
# in the new 'nano' zone.
#
# Those are just warnings, it isn't an error to not use the 'nano' zone.
#
# This disables use of the nano zone which quiets those messages. This
# is only in my terminal and shell sessions, not the system as a whole.

export MallocNanoZone=0

# I've given in to the pressure to switch to Cmake. I default my builds
# to use ninja instead of gnu make because multiple configuration support
# is built in.

export CMAKE_GENERATOR="Ninja Multi-Config"

# My Path extensions. If I've loaded it with brew or whatever, I mean to use
# it and not the system provided versions. This should be safe because any
# system processes won't have my path extensions.

# See back by setting PUREBASIC_HOME for explanation of path to compilers.

typeset -U path PATH
path=(${HOME}/.local/bin \
		$(go env GOPATH)/bin \
    /opt/homebrew/opt \
#    /opt/homebrew/opt/node@20/bin \
    /opt/homebrew/opt/ruby/bin \
    /opt/homebrew/lib/ruby/gems/3.4.0/bin \
    /opt/homebrew/opt/openjdk/bin \
    /Applications/PureBasic.app/Contents/Resources/compilers \
    $path[@])
export PATH

# if you need ruby for the below, add these
#  export LDFLAGS="-L/opt/homebrew/opt/ruby/lib"
#  export CPPFLAGS="-I/opt/homebrew/opt/ruby/include"
#  export PKG_CONFIG_PATH="/opt/homebrew/opt/ruby/lib/pkgconfig"

# if you need compilers to find node ...
#export LDFLAGS="-L/opt/homebrew/opt/node@20/lib $LDFLAGS"
#export CPPFLAGS="-I/opt/homebrew/opt/node@20/include $CPPFLAGS"

# `readline' is needed for Chicken Ccheme's `breadline' egg.
# # For compilers to find readline you may need to set:
# export LDFLAGS="-L/opt/homebrew/Cellar/readline/8.2.13/lib $LDFLAGS"
# export CPPFLAGS="-I/opt/homebrew/Cellar/readline/8.2.13/include $CPPFLAGS"

# For pkg-config to find readline you may need to set:
# export PKG_CONFIG_PATH="/opt/homebrew/Cellar/readline/8.2.13/lib/pkgconfig"

# I'm using OpenJDK.
export JAVA_HOME=$(/usr/libexec/java_home)
# echo 'export PATH="/opt/homebrew/opt/openjdk/bin:$PATH"' >> ~/.zshrc

# For compilers to find openjdk you may need to set:
#  export CPPFLAGS="-I/opt/homebrew/opt/openjdk/include"
