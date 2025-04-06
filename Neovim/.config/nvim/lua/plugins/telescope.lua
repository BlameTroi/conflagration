local M = {
   "nvim-telescope/telescope.nvim",
   tag = "0.1.8", -- or , branch = '0.1.x',
   dependencies = {
      "nvim-lua/plenary.nvim",
      { "nvim-tree/nvim-web-devicons", enabled = vim.g.have_nerd_font },
      "nvim-telescope/telescope-ui-select.nvim",
      {
         "nvim-telescope/telescope-fzf-native.nvim",
         build = "make", -- this should (re)build on install or update
      },
   },
   config = function()
      local telescope = require "telescope"
      local actions = require "telescope.actions"
      telescope.setup {
         defaults = {
            path_display = { "truncate " },
            mappings = {
               i = {
                  ["<C-k>"] = actions.move_selection_previous,
                  ["<C-j>"] = actions.move_selection_next,
                  ["<C-q>"] = actions.send_selected_to_qflist + actions.open_qflist,
               },
            },
         },
         extensions = {
            ["ui-select"] = {
               require("telescope.themes").get_dropdown(),
            },
         },
      }
      -- Enable Telescope extensions if they are installed
      telescope.load_extension "fzf"
      telescope.load_extension "ui-select"

      -- Somethings I found on the web.
      local keymap = vim.keymap
      keymap.set("n", "<leader>ff", "<cmd>Telescope find_files<cr>", { desc = "Fuzzy in cwd" })
      keymap.set("n", "<leader>fr", "<cmd>Telescope oldfiles<cr>", { desc = "Fuzzy in recent" })
      keymap.set("n", "<leader>fs", "<cmd>Telescope live_grep<cr>", { desc = "Find string in cwd" })
      keymap.set("n", "<leader>fc", "<cmd>Telescope grep_string<cr>", { desc = "Find string under cursor in cwd" })

      -- I stole this from kickstart.
      local builtin = require "telescope.builtin"
      keymap.set("n", "<leader>sh", builtin.help_tags, { desc = "[S]earch [H]elp" })
      keymap.set("n", "<leader>sk", builtin.keymaps, { desc = "[S]earch [K]eymaps" })
      keymap.set("n", "<leader>sf", builtin.find_files, { desc = "[S]earch [F]iles" })
      keymap.set("n", "<leader>ss", builtin.builtin, { desc = "[S]earch [S]elect Telescope" })
      keymap.set("n", "<leader>sw", builtin.grep_string, { desc = "[S]earch current [W]ord" })
      keymap.set("n", "<leader>sg", builtin.live_grep, { desc = "[S]earch by [G]rep" })
      keymap.set("n", "<leader>sd", builtin.diagnostics, { desc = "[S]earch [D]iagnostics" })
      keymap.set("n", "<leader>sr", builtin.resume, { desc = "[S]earch [R]esume" })
      keymap.set("n", "<leader>s.", builtin.oldfiles, { desc = '[S]earch Recent Files ("." for repeat)' })
      keymap.set("n", "<leader><leader>", builtin.buffers, { desc = "[ ] Find existing buffers" })

      -- Slightly advanced example of overriding default behavior and theme
      keymap.set("n", "<leader>/", function()
         -- You can pass additional configuration to Telescope to change the theme, layout, etc.
         builtin.current_buffer_fuzzy_find(require("telescope.themes").get_dropdown {
            winblend = 10,
            previewer = false,
         })
      end, { desc = "[/] Fuzzily search in current buffer" })

      -- It's also possible to pass additional configuration options.
      -- See `:help telescope.builtin.live_grep()` for information about particular keys
      vim.keymap.set("n", "<leader>s/", function()
         builtin.live_grep {
            grep_open_files = true,
            prompt_title = "Live Grep in Open Files",
         }
      end, { desc = "[S]earch [/] in Open Files" })

      -- Shortcut for searching your Neovim configuration files
      vim.keymap.set("n", "<leader>sn", function()
         builtin.find_files { cwd = vim.fn.stdpath "config" }
      end, { desc = "[S]earch [N]eovim files" })
   end,
}
return M
