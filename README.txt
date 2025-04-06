			    CONFLAGRATION
	 The Fires of Eternal Configuration Burn Ever Higher


What the Actual Frak?

_chez moi_ wasn't getting it done for me. I don't think it's a bad
system, but I am unwilling to change my _stow_ and _git_ based habits.
So I'm starting over with a clean repository to get rid of all the
cruft that accumulated in my prior repositories. Configurations for
software I know I won't use again has been removed, as have notes and
script snippets.

Gone are Emacs and Vim. I wanted so much to use Emacs but it doesn't
do what I want easily and I'm tired of wrestling with it. Vim is
viable but all the new work is going into Neovim. Since Vim is
changing its script engine, compatability is going to degrade.

There are several other bits of this that are no longer in use, Ctags
has been replaced by LSP, and so on. I'll be cleaning such things out.

What's in the Box

Since I'm using *nix dotfile names for hidden files, here's a
reasonably current tree showing what's available.

Directories such as bash, astyle, and Neovim are stow packages. If you
stow the package, symbolic links to the contents of the package are
created in the target.

NOTE: Stow will not destroy existing files. Watch what your doing to
be sure the things you think you did were done.



But What Do I Do?

I'm using GNU stow to push stuff out where it needs to be seen. Sub
directories for "packages" such as Neovim contain the files and
directory structure for each package. Many of the sub directories are
hidden in the Unix fashion but stow sees them fine. The spelling out
of "dot" in _chez moi_ was one of the things I didn't like about it. I
know stow also provides a =--dotfiles= option, but I don't use it.

Clone the repository and then install the packages you wish.

stow --(re)stow --target=$HOME emacs vim ...

Remember that stow won't destroy your existing files and links.


Authorship, Licensing, and Copyright

Like almost everyone else, I've borrowed from _reddit_, _github_,
_stackoverflow_, and points far and wide. There's some good stuff out
there, but I've given up trying to keep track of where I got stuff and
provide attribution. I'm not selling anything. Use as you see fit. My
stuff is public domain, but the MIT license is offered as an
additional option if you like. See the LICENSE file for details.


Troy Brumley blametroi@gmail.com

So let it be written.  
So let it be done.
