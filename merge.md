# Merge configuration changes

I'm overdue for a cleanup of this Conflagration.

Moving orphaned dot files from `$HOME` into proper Stow packages. I prefer to package by application even though I will tend to `--stow` all of these.

The command is:

```sh
stow --(re)stow --target=$HOME emacs vim ...
```

This will either create or correct symbolic links from where applications expect to find configuration files to the actual files here.

If there a file already exists in the target location, Stow will not overwrite it. You would need to manually delete the duplicate file (or merge it) and issue the `stow` command again.

## Packages to Stow

- `clang`
- `clangd`
- `clang-format`
- `kitty`
- `astyle`
- `root-dots`
- `git`
- `bash`
- `zsh`
- `editorconfig`
- `harper-ls`
- `helix`
- `dprint`

## File Naming and Location Oddities

Everything is everywhere all at once!

Many but not all applications use the XDG specification. A few of the newer applications use project or workspace roots. A very few follow the recommended standards for Windows and macOS. And some stragglers still follow the hidden file in $HOME convention.

My default/global settings will all be managed here. Project level overrides will be managed within each project. I don't expect to use many overrides in my code, but if I am contributing to public code I should use the settings the author prefers.

Applications such as `Cmake` and `Clang` are immature, inconsistent, and in flux.

Shell configuration files are in the user home as always.

### `clang-format`

User home.

### `astyle`

User home.

### `editorconfig`

User home. 

### `dprint`

User home.

### `harper-ls`

System standard location. This is `~/Library/Application Support/harper-ls/` on macOS.
