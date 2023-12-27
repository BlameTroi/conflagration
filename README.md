# conflagration -- the fires of eternal configuration burn ever higher

_chez moi_ wasn't getting it done for me. I don't think it's a bad system, but I am unwilling to change my _stow_ and _git_ based habits. So I'm starting over with a clean repository to get rid of all the cruft that accumulated in my prior repositories. Configurations for software I know I won't use again has been removed, as have notes and script snippets.

My Emacs configuration is in the literate style using org-babel, and I might try to make more of my configurations literate.

## WHAT'S IN THE BOX

I'm using GNU stow to push stuff out where it needs to be seen. Sub directories for "packages" such as emacs or vim contain the directory structure and files for each package. Many of the sub directories are hidden in the Unix fashion but stow sees them fine. The spelling out of "dot" in _chez moi_ was one of the things I didn't like about it. I know stow also provides a `--dotfiles` option, but I don't use it. 

Clone the repository and then install the packages you wish.

``` shell
stow --(re)stow --target=$HOME emacs vim ...
```

Remember that stow won't destroy your existing files and links.

## AUTHORSHIP, LICENSING, AND COPYRIGHT

Like almost everyone else, I've borrowed from _reddit_, _stackoverflow_, _github_, and points far and wide. There's some good stuff out there, but I've given up trying to keep track of where I got stuff and provide attribution. I'm not selling anything. Use as you see fit. My stuff is public domain.

Troy Brumley
blametroi@gmail.com

So let it be written. So let it be done.
