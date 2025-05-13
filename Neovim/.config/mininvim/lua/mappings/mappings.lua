-- ~/.config/mininvim/lua/mappings/mappings.lua

---Uncomment below to double check that your only undefined globals
---are those you trust: eg, vim.*, MiniDeps.*.
---@diagnostic disable:undefined-global

-- Keymaps.

-- Originally these were pulled from LazyVim's defaults and then I did some
-- tweaks. As I'm switching to mini.nvim, I will take the mini.basics keys
-- and add or change here as needed.

-- Move Lines

vim.keymap.set("n", "<A-j>", "<cmd>execute 'move .+' . v:count1<cr>==", { desc = "Move Down" })
vim.keymap.set("n", "<A-k>", "<cmd>execute 'move .-' . (v:count1 + 1)<cr>==", { desc = "Move Up" })
vim.keymap.set("i", "<A-j>", "<esc><cmd>m .+1<cr>==gi", { desc = "Move Down" })
vim.keymap.set("i", "<A-k>", "<esc><cmd>m .-2<cr>==gi", { desc = "Move Up" })
vim.keymap.set("v", "<A-j>", ":<C-u>execute \"'<,'>move '>+\" . v:count1<cr>gv=gv", { desc = "Move Down" })
vim.keymap.set("v", "<A-k>", ":<C-u>execute \"'<,'>move '<-\" . (v:count1 + 1)<cr>gv=gv", { desc = "Move Up" })

-- Clear search, diff update and redraw
-- taken from runtime/lua/_editor.lua
vim.keymap.set(
   "n",
   "<leader>ur",
   "<Cmd>nohlsearch<Bar>diffupdate<Bar>normal! <C-L><CR>",
   { desc = "Redraw / Clear hlsearch / Diff Update" }
)

-- https://github.com/mhinz/vim-galore#saner-behavior-of-n-and-n
-- n always searches next forward, N always searches previous backward
vim.keymap.set("n", "n", "'Nn'[v:searchforward].'zv'", { expr = true, desc = "Next Search Result" })
vim.keymap.set("x", "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next Search Result" })
vim.keymap.set("o", "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next Search Result" })
vim.keymap.set("n", "N", "'nN'[v:searchforward].'zv'", { expr = true, desc = "Prev Search Result" })
vim.keymap.set("x", "N", "'nN'[v:searchforward]", { expr = true, desc = "Prev Search Result" })
vim.keymap.set("o", "N", "'nN'[v:searchforward]", { expr = true, desc = "Prev Search Result" })

-- save file
vim.keymap.set({ "i", "x", "n", "s" }, "<C-s>", "<cmd>w<cr><esc>", { desc = "Save File" })

-- keywordprg
vim.keymap.set("n", "<leader>K", "<cmd>norm! K<cr>", { desc = "Keywordprg" })

-- better indenting
vim.keymap.set("v", "<", "<gv")
vim.keymap.set("v", ">", ">gv")

-- location list
vim.keymap.set("n", "<leader>xl", function()
   local success, err = pcall(vim.fn.getloclist(0, { winid = 0 }).winid ~= 0 and vim.cmd.lclose or vim.cmd.lopen)
   if not success and err then vim.notify(err, vim.log.levels.ERROR) end
end, { desc = "Location List" })

-- quickfix list
vim.keymap.set("n", "<leader>xq", function()
   local success, err = pcall(vim.fn.getqflist({ winid = 0 }).winid ~= 0 and vim.cmd.cclose or vim.cmd.copen)
   if not success and err then vim.notify(err, vim.log.levels.ERROR) end
end, { desc = "Quickfix List" })

vim.keymap.set("n", "[q", vim.cmd.cprev, { desc = "Previous Quickfix" })
vim.keymap.set("n", "]q", vim.cmd.cnext, { desc = "Next Quickfix" })

-- -- diagnostic
-- NOTE: When I uncomment out this code block, some messages get logged during
--       initialization. It may be related to TS.
-- local diagnostic_goto = function(next, severity)
--   local go = next and vim.diagnostic.jump({ count = 1, float = true })
--     or vim.diagnostic.jump({ count = -1, float = true })
--   severity = severity and vim.diagnostic.severity[severity] or nil
--   return function() go({ severity = severity }) end
-- end
-- map("n", "<leader>cd", vim.diagnostic.open_float, { desc = "Line Diagnostics" })
-- map("n", "]d", diagnostic_goto(true), { desc = "Next Diagnostic" })
-- map("n", "[d", diagnostic_goto(false), { desc = "Prev Diagnostic" })
-- map("n", "]e", diagnostic_goto(true, "ERROR"), { desc = "Next Error" })
-- map("n", "[e", diagnostic_goto(false, "ERROR"), { desc = "Prev Error" })
-- map("n", "]w", diagnostic_goto(true, "WARN"), { desc = "Next Warning" })
-- map("n", "[w", diagnostic_goto(false, "WARN"), { desc = "Prev Warning" })

-- stylua: ignore start

-- highlights under cursor
vim.keymap.set("n", "<leader>ui", vim.show_pos, {desc = "Inspect Pos"})
vim.keymap.set("n", "<leader>uI", function()
    vim.treesitter.inspect_tree()
    vim.api.nvim_input("I")
end, {desc = "Inspect Tree"})

--
-- Terminal Mappings
vim.keymap.set("t", "<C-/>", "<cmd>close<cr>", {desc = "Hide Terminal"})
vim.keymap.set("t", "<c-_>", "<cmd>close<cr>", {desc = "which_key_ignore"})

-- windows
vim.keymap.set("n", "<leader>-", "<C-W>s", {desc = "Split Window Below", remap = true})
vim.keymap.set("n", "<leader>|", "<C-W>v", {desc = "Split Window Right", remap = true})
vim.keymap.set("n", "<leader>wd", "<C-W>c", {desc = "Delete Window", remap = true})

-- tabs
vim.keymap.set("n", "<leader><tab>l", "<cmd>tablast<cr>", {desc = "Last Tab"})
vim.keymap.set("n", "<leader><tab>o", "<cmd>tabonly<cr>", {desc = "Close Other Tabs"})
vim.keymap.set("n", "<leader><tab>f", "<cmd>tabfirst<cr>", {desc = "First Tab"})
vim.keymap.set("n", "<leader><tab><tab>", "<cmd>tabnew<cr>", {desc = "New Tab"})
vim.keymap.set("n", "<leader><tab>]", "<cmd>tabnext<cr>", {desc = "Next Tab"})
vim.keymap.set("n", "<leader><tab>d", "<cmd>tabclose<cr>", {desc = "Close Tab"})
vim.keymap.set("n", "<leader><tab>[", "<cmd>tabprevious<cr>", {desc = "Previous Tab"})

-- Add any additional keymaps here

-- Exit terminal mode in the builtin terminal with an easier to remember
-- shortcut.

vim.keymap.set("t", "<Esc><Esc>", "<C-\\><C-n>", {desc = "Exit terminal mode"})

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
