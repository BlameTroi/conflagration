			    CONFLAGRATION
	 The Fires of Eternal Configuration Burn Ever Higher


What the Actual Frak?

/chez moi/ wasn't getting it done for me. I don't think it's a bad
system, but I am unwilling to change my /stow/ and /git/ based habits.
So I'm starting over with a clean repository to get rid of all the
cruft that accumulated in my prior repositories. Configurations for
software I know I won't use again has been removed, as have notes and
script snippets.

My Emacs configuration was in the literate style using org-babel, but
I finally relented and installed Doom Emacs. Then I decided that Doom
Emacs wasn't teaching me anything and it was too big for what I do, so
now my Emacs configurations are split out while I'm working on them.


What's in the Box

Since I'm using *nix dotfile names for hidden files, here's a
reasonably current tree showing what's available. vim and nvim have
not been updated in a while, fpc is out of use, and my emacs here is
currently stale until I get things quieted down.

Directories such as bash, astyle, or ctags are stow packages. If you
stow the package, symbolic links to the contents of the package are
created in the target.

.
├── .gitignore
├── LICENSE
├── README.txt
├── astyle
│   └── .astylerc
├── bash
│   ├── .bashrc
│   └── .profile
├── ctags
│   └── .config
│       └── ctags
│           ├── 10_excludes.ctags
│           ├── 20_c.ctags
│           ├── 30_lua.ctags
│           ├── 40_pascal.ctags
│           └── 50_vim.ctags
├── emacs
│   └── .emacs.d
│       ├── init.el
│       ├── literate
│       │   ├── literate-config.el
│       │   └── literate-config.org
│       ├── site-lisp
│       │   ├── advent.el
│       │   ├── fortune-cookie.el
│       │   ├── idcase.el
│       │   ├── init-abbrev-f90.el
│       │   ├── init-f90.el
│       │   ├── my-pascal-helper.el
│       │   └── pascal.el
│       └── snippets
│           ├── pascal-mode
│           │   ├── aoc-input.yas
│           │   ├── include-frame.yas
│           │   └── program-frame.yas
│           └── snippets -> snippets
├── fpc
│   ├── .fpc.cfg
│   └── fpc.cfg
├── gdb
│   ├── .config
│   │   └── gdb
│   │       └── gdbinit
│   └── .gdbinit
├── git
│   └── .config
│       └── git
│           ├── config
│           └── ignore
├── karbiner-elements
│   └── karabiner.json
├── miscellany
│   ├── 3279-colors.txt
│   ├── makeKittyDefault.txt
│   ├── map-capslock-control.sh
│   ├── notes-jmoyers.md
│   ├── notes.txt
│   ├── project-level-exrc-clang.vim
│   ├── project-level-exrc-cmake.vim
│   ├── project-level-exrc-gcc.vim
│   └── rsync-home-excludes.txt
├── nvim
│   └── .config
│       └── nvim
│           ├── after
│           │   ├── compiler
│           │   │   └── fpc.vim
│           │   └── ftplugin
│           │       └── markdown.vim
│           ├── init.vim
│           └── plugin
│               ├── dmsg.vim
│               ├── functional-prog.vim
│               ├── grep-operator.vim
│               └── toggles.vim
├── vim
│   ├── .vim
│   │   ├── autoload
│   │   │   └── plug.vim
│   │   ├── colors
│   │   │   └── crt3270.vim
│   │   ├── filetype.vim
│   │   ├── ftplugin
│   │   │   └── z80
│   │   │       └── z80.vim
│   │   ├── indent
│   │   │   └── basic.vim
│   │   └── syntax
│   │       ├── basic.vim
│   │       ├── isopascal.vim
│   │       ├── libertybasic.vim
│   │       └── z80.vim
│   └── .vimrc
└── zsh
    ├── .zprofile
    ├── .zshenv
    └── .zshrc


But What Do I Do?

I'm using GNU stow to push stuff out where it needs to be seen. Sub
directories for "packages" such as emacs or vim contain the directory
structure and files for each package. Many of the sub directories are
hidden in the Unix fashion but stow sees them fine. The spelling out
of "dot" in /chez moi/ was one of the things I didn't like about it. I
know stow also provides a =--dotfiles= option, but I don't use it.

Clone the repository and then install the packages you wish.

stow --(re)stow --target=$HOME emacs vim ...

Remember that stow won't destroy your existing files and links.


Authorship, Licensing, and Copyright

Like almost everyone else, I've borrowed from /reddit/,
/stackoverflow/, /github/, and points far and wide. There's some good
stuff out there, but I've given up trying to keep track of where I got
stuff and provide attribution. I'm not selling anything. Use as you
see fit. My stuff is public domain, but the MIT license is offered as
an additional option if you like. See the LICENSE file for details.


Troy Brumley blametroi@gmail.com

So let it be written. So let it be done.
