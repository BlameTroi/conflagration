-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

-- Exit terminal mode in the builtin terminal with an easier to remember
-- shortcut.

vim.keymap.set("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })

-- I turn off arrow keys in normal mode to avoid mouse/touchpad scrolling. They
-- are still on for insert and visual mode, but I may someday be able to turn
-- them and the mouse completely off in Neovim.

vim.keymap.set("n", "<left>", "")
vim.keymap.set("n", "<right>", "")
vim.keymap.set("n", "<up>", "")
vim.keymap.set("n", "<down>", "")

-- Trying to learn to not use arrow keys in insert or visual mode. The
-- power users jump out and use motion commands.

vim.keymap.set("i", "<left>", "")
vim.keymap.set("i", "<right>", "")
vim.keymap.set("i", "<up>", "")
vim.keymap.set("i", "<down>", "")

vim.keymap.set("v", "<left>", "")
vim.keymap.set("v", "<right>", "")
vim.keymap.set("v", "<up>", "")
vim.keymap.set("v", "<down>", "")
