-- ~/.config/mininvim/lua/plugins/init.lua

-- Help Lua Language Server find things to avoid false errors.
require("plugins.lazydev")

-- Git helpers for sign column marking, hunk navigation, etc.
require("plugins.gitsigns")

-- Those things for writting prose. Mostly just 512-words.
require("plugins.prose")

-- File and operating system tools.
require("plugins.files")

-- Diff utilities.
require("plugins.diffs")

-- Pickers choosers and finders.
require("plugins.picker")
