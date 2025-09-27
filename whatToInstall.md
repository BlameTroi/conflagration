# Repaving an ubuntu system.

What to install, how, and in what rough order.

## Update software sources

Add alternate sources. Most are via a settings UI, but some are
done via apt-add-repository. Security keys may be required, so
check the web for current keys and URLs. This gets us the latest
vim, neovim, and wine.

* Ubuntu Software
** community maintained foss (universe)
** proprietary drivers (restricted)
** software restricted by copyright or legal issues (multiverse)

* Other Software:
** https://dl.winehq.wine-builds/ubuntu/ focal main
** http://ppa.launchpad.net/git-core/ppa/ubuntu/ focal main
** http://ppa.launchpad.net/jonathonf/vim/ubuntu focal main
** http://ppa.launchpad.net/neovim-ppa/stable/ubuntu focal main

* Proprietary Drivers:
** allow to get nvidia proprietary tested drivers

## Install basic tools.

Get these, mostly via apt.

zsh
build-essential
make-doc
curl
file
git
automake
autoconf
flex
bison
gdb
cgdb
libncurses-dev
ncurses-doc
ncurses-examples
cups (if not already installed)
canon printer drivers from their website

ripgrep (via deb from github home)
fd-find
fzf

vim
neovim

grsync

python3 python3-dev python3-doc python3-tk python3-venv python3-examples
tk8.6 tk8.6-dev tcl8.6-dev tcl8.6-doc tk8.6-doc
python3-tk-dbg

winehq-stable (after setting source above), add --install-recommends
lutris (via deb from github home)

fpc via download from freepascal.org

## Cleanup.

Remove lubuntu-update-notifier, as you can't turn off the
nag popups any other way.

and do an autoremove if needed.

