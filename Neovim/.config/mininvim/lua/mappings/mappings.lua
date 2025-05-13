-- ~/.config/mininvim/lua/mappings/mappings.lua

---Uncomment below to double check that your only undefined globals
---are those you trust: eg, vim.*, MiniDeps.*.
--- @diagnostic disable:undefined-global

-- I use the key mappings from mini.basics as a starting point, and then add
-- some of my own.

-- Intuitive line movement in a buffer -------------------------------

-- I do not use the "move_with_alt" setting of mini.basics, which offers
-- <A-hjkl> as alternatives to arrows for cursor movement. I prefer to use the
-- <A-hjkl> for moving lines in a buffer.

vim.keymap.set("n", "<A-j>", "<cmd>execute 'move .+' . v:count1<cr>==", { desc = "Move Down" })
vim.keymap.set("n", "<A-k>", "<cmd>execute 'move .-' . (v:count1 + 1)<cr>==", { desc = "Move Up" })
vim.keymap.set("i", "<A-j>", "<esc><cmd>m .+1<cr>==gi", { desc = "Move Down" })
vim.keymap.set("i", "<A-k>", "<esc><cmd>m .-2<cr>==gi", { desc = "Move Up" })
vim.keymap.set("v", "<A-j>", ":<C-u>execute \"'<,'>move '>+\" . v:count1<cr>gv=gv", { desc = "Move Down" })
vim.keymap.set("v", "<A-k>", ":<C-u>execute \"'<,'>move '<-\" . (v:count1 + 1)<cr>gv=gv", { desc = "Move Up" })

-- Better search next/previous mappings ------------------------------

-- Instead of trying to remember different keys for different search directions,
-- n always searches next forward, N always searches previous backward.

vim.keymap.set("n", "n", "'Nn'[v:searchforward].'zv'", { expr = true, desc = "Next Search Result" })
vim.keymap.set("x", "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next Search Result" })
vim.keymap.set("o", "n", "'Nn'[v:searchforward]", { expr = true, desc = "Next Search Result" })
vim.keymap.set("n", "N", "'nN'[v:searchforward].'zv'", { expr = true, desc = "Prev Search Result" })
vim.keymap.set("x", "N", "'nN'[v:searchforward]", { expr = true, desc = "Prev Search Result" })
vim.keymap.set("o", "N", "'nN'[v:searchforward]", { expr = true, desc = "Prev Search Result" })

-- Save file using the CUA standard key ------------------------------

vim.keymap.set({ "i", "x", "n", "s" }, "<C-s>", "<cmd>w<cr><esc>", { desc = "Save File" })

-- Tell me about the word under the cursor ---------------------------

-- LSP support steals K, moving it from :keywordprg to vim.lsp.buf.hover(). Use
-- <leader>K for :keywordprg. I have to do some work to use it for more than
-- just :man.

vim.keymap.set("n", "<leader>K", "<cmd>norm! K<cr>", { desc = "Keywordprg" })

-- Open location list ------------------------------------------------

vim.keymap.set("n", "<leader>xl", function()
   local success, err = pcall(vim.fn.getloclist(0, { winid = 0 }).winid ~= 0 and vim.cmd.lclose or vim.cmd.lopen)
   if not success and err then vim.notify(err, vim.log.levels.ERROR) end
end, { desc = "Location List" })

-- Open quickfix list ------------------------------------------------

vim.keymap.set("n", "<leader>xq", function()
   local success, err = pcall(vim.fn.getqflist({ winid = 0 }).winid ~= 0 and vim.cmd.cclose or vim.cmd.copen)
   if not success and err then vim.notify(err, vim.log.levels.ERROR) end
end, { desc = "Quickfix List" })

-- Diagnostic Navigation does not appear to be needed anymore --------
--
-- vim.keymap.set("n", "[q", vim.cmd.cprev, { desc = "Previous Quickfix" })
-- vim.keymap.set("n", "]q", vim.cmd.cnext, { desc = "Next Quickfix" })
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

-- Treesitter information on item under cursor------------------------

vim.keymap.set("n", "<leader>ui", vim.show_pos, { desc = "Inspect Pos" })
vim.keymap.set("n", "<leader>uI", function()
   vim.treesitter.inspect_tree()
   vim.api.nvim_input("I")
end, { desc = "Inspect Tree" })

-- Window splits -----------------------------------------------------

vim.keymap.set("n", "<leader>-", "<C-W>s", { desc = "Split Window Below", remap = true })
vim.keymap.set("n", "<leader>|", "<C-W>v", { desc = "Split Window Right", remap = true })
vim.keymap.set("n", "<leader>wd", "<C-W>c", { desc = "Delete Window", remap = true })

-- Tab navigation ----------------------------------------------------

-- I don't use tabs at present. Leaving as comments for future reference.
-- vim.keymap.set("n", "<leader><tab>l", "<cmd>tablast<cr>", { desc = "Last Tab" })
-- vim.keymap.set("n", "<leader><tab>o", "<cmd>tabonly<cr>", { desc = "Close Other Tabs" })
-- vim.keymap.set("n", "<leader><tab>f", "<cmd>tabfirst<cr>", { desc = "First Tab" })
-- vim.keymap.set("n", "<leader><tab><tab>", "<cmd>tabnew<cr>", { desc = "New Tab" })
-- vim.keymap.set("n", "<leader><tab>]", "<cmd>tabnext<cr>", { desc = "Next Tab" })
-- vim.keymap.set("n", "<leader><tab>d", "<cmd>tabclose<cr>", { desc = "Close Tab" })
-- vim.keymap.set("n", "<leader><tab>[", "<cmd>tabprevious<cr>", { desc = "Previous Tab" })

-- Terminal Mappings -------------------------------------------------

vim.keymap.set("t", "<C-/>", "<cmd>close<cr>", { desc = "Hide Terminal" })
vim.keymap.set("t", "<Esc><Esc>", "<C-\\><C-n>", { desc = "Exit terminal mode" })

-- "Hard" mode Mappings -------------------------------------------------

-- Turn off arrow keys in normal mode to avoid mouse/touchpad scrolling.

vim.keymap.set("n", "<left>", "")
vim.keymap.set("n", "<right>", "")
vim.keymap.set("n", "<up>", "")
vim.keymap.set("n", "<down>", "")

-- Turn off arrow keys for input and visual mode. Use the proper movements.
-- Spend most of your time in normal mode.

vim.keymap.set("i", "<left>", "")
vim.keymap.set("i", "<right>", "")
vim.keymap.set("i", "<up>", "")
vim.keymap.set("i", "<down>", "")

vim.keymap.set("v", "<left>", "")
vim.keymap.set("v", "<right>", "")
vim.keymap.set("v", "<up>", "")
vim.keymap.set("v", "<down>", "")
