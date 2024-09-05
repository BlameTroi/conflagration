* Emacs mah na mah na

As I've done more with Emacs, the configuration tweaks grow, and it's time yet again for another iteration. The beat, as they say, goes on.

For programming I'm mostly using C and Fortran. The other languages are Pascal, Ruby, Scheme, and Cobol. Eglot, Treesitter, astyle, and other bits of support for them are all working.

For writing I will use org and markdown.

On the off chance something in here is new, it's all public domain as far as I'm concerned. You can use this software either as public domain under the unlicense or under the terms of the MIT license.

Troy Brumley  
Blametroi@gmail.com  
May 2024  

So let it be written, so let it be done.

** Philosophy and structure

I will do my best to use base Emacs, so at least version 29 is required. For package and repositories, use-package is the logical choice now that it's built in to Emacs. Similarly, eglot over lsp-mode.

I originally didn't like, and am still not fond of, the customization interface. It's gotten better but the line between what should be in their and what shouldn't can be hard to see. Generally, if it's a package I load, the configuration is via use-package. Simple options in the base system tend to end up in customization.

** Influences and references

Like all Emacs users, I keep fiddling with configuration. I read somewhere that it's a "tinkers' editor" and I would have to agree. Vim is as well, but Emacs seems moreso.

Over the years I've seen many configs and read many articles and blog posts. However, things recently clicked for me after three experiences:

1. Frustrated with Vim and Neovim, I decided to try Doom Emacs. This was more for the config than keymaps. I decided that while interesting, Doom was simultaneously too big and too insular. You get a lot of extras without knowing what they are really for or why you should use them.

2. Frustrated by Doom but preferring Emacs over Vim of late, I stumbled upon [Emacs-Bedrock](https://sr.ht/~ashton314/emacs-bedrock/). Wiersdorf's put together a great configuration. I started working through it and modifying it but then while looking at some Tree Sitter and eglot issues, I found ...

3. [From Zero to IDE with Emacs and LSP](https://justinbarclay.ca/posts/from-zero-to-ide-with-emacs-and-lsp/). This post made it all become very clear to me and something wonderful happened.

And so I'm *really* rolling my own Emacs configuration. Fun has commenced. 

#+CAPTION: The muppets bring you the ultimate ear worm
#+NAME:   happiness
[[./keep-calm-and-mahna-mahna.png]]
