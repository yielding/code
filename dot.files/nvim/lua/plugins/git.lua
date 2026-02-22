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
        watch_gitdir = {
          follow_files = true,
        },
        current_line_blame = false,
        current_line_blame_opts = {
          virt_text = true,
          virt_text_pos = "eol",
          delay = 1000,
        },
        sign_priority = 6,
        update_debounce = 100,
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
      vim.keymap.set("n", "<leader>td", gitsigns.preview_hunk_inline, { desc = "Toggle deleted" })

      -- Navigation
      vim.keymap.set("n", "]c", function()
        if vim.wo.diff then return "]c" end
        vim.schedule(function() gitsigns.nav_hunk("next") end)
        return "<Ignore>"
      end, { expr = true, desc = "Next hunk" })

      vim.keymap.set("n", "[c", function()
        if vim.wo.diff then return "[c" end
        vim.schedule(function() gitsigns.nav_hunk("prev") end)
        return "<Ignore>"
      end, { expr = true, desc = "Previous hunk" })

      -- Actions
      vim.keymap.set("n", "<leader>hs", gitsigns.stage_hunk, { desc = "Stage hunk" })
      vim.keymap.set("n", "<leader>hr", gitsigns.reset_hunk, { desc = "Reset hunk" })
      vim.keymap.set("v", "<leader>hs", function() gitsigns.stage_hunk({ vim.fn.line("."), vim.fn.line("v") }) end, { desc = "Stage hunk" })
      vim.keymap.set("v", "<leader>hr", function() gitsigns.reset_hunk({ vim.fn.line("."), vim.fn.line("v") }) end, { desc = "Reset hunk" })
      vim.keymap.set("n", "<leader>hS", gitsigns.stage_buffer, { desc = "Stage buffer" })
      vim.keymap.set("n", "<leader>hu", gitsigns.stage_hunk, { desc = "Undo stage hunk" })
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
    cmd = { "Git", "Gstatus", "Gblame", "Gpush", "Gpull", "Gdiff", "Gvdiffsplit" },
    keys = {
      { "<leader>2", "<cmd>diffget //2<cr>", desc = "Diffget from target (//2)" },
      { "<leader>3", "<cmd>diffget //3<cr>", desc = "Diffget from merge (//3)" },
    },
  },

  -- gitv: Git log viewer
  {
    "vim-scripts/gitv",
    cmd = { "Gitv" },
    dependencies = { "tpope/vim-fugitive" },
  },

  -- git-worktree: Git worktree management
  {
    "ThePrimeagen/git-worktree.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
    },
    config = function()
      require("git-worktree").setup()
      require("telescope").load_extension("git_worktree")
    end,
    keys = {
      { "<leader>gw", function() require("telescope").extensions.git_worktree.git_worktrees() end, desc = "Git Worktrees" },
      { "<leader>gW", function() require("telescope").extensions.git_worktree.create_git_worktree() end, desc = "Create Worktree" },
    },
  },

  -- diffview: Diff viewer
  {
    "sindrets/diffview.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    cmd = { "DiffviewOpen", "DiffviewFileHistory" },
    keys = {
      { "<leader>dv", "<cmd>DiffviewOpen<cr>", desc = "Diffview Open" },
      { "<leader>dh", "<cmd>DiffviewFileHistory %<cr>", desc = "Diffview File History" },
      { "<leader>dH", "<cmd>DiffviewFileHistory<cr>", desc = "Diffview Branch History" },
      { "<leader>dc", "<cmd>DiffviewClose<cr>", desc = "Diffview Close" },
    },
    config = true,
  },

  -- neogit
  {
    "NeogitOrg/neogit",
    dependencies = {
      "nvim-lua/plenary.nvim",         -- 필수 의존성
      "sindrets/diffview.nvim",        -- 선택 사항: Diff 뷰어 통합
      "nvim-telescope/telescope.nvim", -- 선택 사항: 메뉴 탐색 통합
    },
    cmd = { "Neogit" },
    config = true,
    keys = {
      { "<leader>gn", "<cmd>Neogit<cr>", desc = "Neogit" },
    },
  },
}
