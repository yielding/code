-- lua/plugins/git.lua
-- Git integration

return {
  -- gitsigns: Git decorations
  {
    "lewis6991/gitsigns.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("gitsigns").setup({
        signs = {
          add = { text = "│" },
          change = { text = "│" },
          delete = { text = "_" },
          topdelete = { text = "‾" },
          changedelete = { text = "~" },
          untracked = { text = "┆" },
        },
        signcolumn = true,
        numhl = false,
        linehl = false,
        word_diff = false,
        watch_gitdir = {
          follow_files = true,
        },
        attach_to_untracked = true,
        current_line_blame = false,
        current_line_blame_opts = {
          virt_text = true,
          virt_text_pos = "eol",
          delay = 1000,
        },
        sign_priority = 6,
        update_debounce = 100,
        status_formatter = nil,
        max_file_length = 40000,
        preview_config = {
          border = "single",
          style = "minimal",
          relative = "cursor",
          row = 0,
          col = 1,
        },
      })

      -- Toggle functions for gitsigns
      local gitsigns = require("gitsigns")

      vim.keymap.set("n", "<leader>tg", gitsigns.toggle_signs, { desc = "Toggle git signs" })
      vim.keymap.set("n", "<leader>tl", gitsigns.toggle_linehl, { desc = "Toggle line highlight" })
      vim.keymap.set("n", "<leader>tn", gitsigns.toggle_numhl, { desc = "Toggle number highlight" })
      vim.keymap.set("n", "<leader>tb", gitsigns.toggle_current_line_blame, { desc = "Toggle line blame" })
      vim.keymap.set("n", "<leader>td", gitsigns.toggle_deleted, { desc = "Toggle deleted" })

      -- Navigation
      vim.keymap.set("n", "]c", function()
        if vim.wo.diff then return "]c" end
        vim.schedule(function() gitsigns.next_hunk() end)
        return "<Ignore>"
      end, { expr = true, desc = "Next hunk" })

      vim.keymap.set("n", "[c", function()
        if vim.wo.diff then return "[c" end
        vim.schedule(function() gitsigns.prev_hunk() end)
        return "<Ignore>"
      end, { expr = true, desc = "Previous hunk" })

      -- Actions
      vim.keymap.set("n", "<leader>hs", gitsigns.stage_hunk, { desc = "Stage hunk" })
      vim.keymap.set("n", "<leader>hr", gitsigns.reset_hunk, { desc = "Reset hunk" })
      vim.keymap.set("v", "<leader>hs", function() gitsigns.stage_hunk({ vim.fn.line("."), vim.fn.line("v") }) end, { desc = "Stage hunk" })
      vim.keymap.set("v", "<leader>hr", function() gitsigns.reset_hunk({ vim.fn.line("."), vim.fn.line("v") }) end, { desc = "Reset hunk" })
      vim.keymap.set("n", "<leader>hS", gitsigns.stage_buffer, { desc = "Stage buffer" })
      vim.keymap.set("n", "<leader>hu", gitsigns.undo_stage_hunk, { desc = "Undo stage hunk" })
      vim.keymap.set("n", "<leader>hR", gitsigns.reset_buffer, { desc = "Reset buffer" })
      vim.keymap.set("n", "<leader>hp", gitsigns.preview_hunk, { desc = "Preview hunk" })
      vim.keymap.set("n", "<leader>hb", function() gitsigns.blame_line({ full = true }) end, { desc = "Blame line" })
      vim.keymap.set("n", "<leader>hd", gitsigns.diffthis, { desc = "Diff this" })
      vim.keymap.set("n", "<leader>hD", function() gitsigns.diffthis("~") end, { desc = "Diff this ~" })
    end,
  },

  -- fugitive: Git commands
  {
    "tpope/vim-fugitive",
    cmd = { "Git", "Gstatus", "Gblame", "Gpush", "Gpull", "Gdiff" },
  },

  -- gitv: Git log viewer
  {
    "vim-scripts/gitv",
    cmd = { "Gitv" },
    dependencies = { "tpope/vim-fugitive" },
  },

  -- git-worktree: Git worktree management
  {
    "polarmutex/git-worktree.nvim",
    version = "^2",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
    },
    config = function()
      require("telescope").load_extension("git_worktree")

      local Hooks = require("git-worktree.hooks")
      Hooks.register(Hooks.type.SWITCH, Hooks.builtins.update_current_buffer_on_switch)
    end,
    keys = {
      { "<leader>gw", function() require("telescope").extensions.git_worktree.git_worktree() end, desc = "Git worktrees" },
      { "<leader>gW", function() require("telescope").extensions.git_worktree.create_git_worktree() end, desc = "Create git worktree" },
    },
  },

  -- neogit: Magit-like git interface
  {
    "NeogitOrg/neogit",
    cmd = "Neogit",
    keys = {
      { "<leader>gn", "<cmd>Neogit<CR>", desc = "Neogit" },
    },
    dependencies = {
      "nvim-lua/plenary.nvim",
      "sindrets/diffview.nvim",
    },
    opts = {
      integrations = {
        diffview = true,
      },
    },
  },
}
