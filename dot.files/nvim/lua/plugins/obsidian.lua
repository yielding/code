-- lua/plugins/obsidian.lua
-- Obsidian.nvim integration

local vault_path = vim.fn.expand("~") .. "/Library/Mobile Documents/iCloud~md~obsidian/Documents/Notes"

return {
  {
    "epwalsh/obsidian.nvim",
    version = "*",
    lazy = true,
    event = {
      "BufReadPre " .. vault_path .. "/**.md",
      "BufNewFile " .. vault_path .. "/**.md",
    },
    dependencies = {
      "nvim-lua/plenary.nvim",
    },
    config = function(_, opts)
      vim.api.nvim_create_autocmd("BufEnter", {
        pattern = vault_path .. "/**.md",
        callback = function()
          vim.opt_local.conceallevel = 2
        end,
      })
      require("obsidian").setup(opts)
    end,
    opts = {
      workspaces = {
        {
          name = "vault",
          path = vault_path,
        },
      },
      notes_subdir = "0. Inbox",
      new_notes_location = "notes_subdir",
      ui = {
        checkboxes = {
          [" "] = { char = "󰄱", hl_group = "ObsidianTodo" },
          ["x"] = { char = "", hl_group = "ObsidianDone" },
          ["/"] = { char = "󰥔", hl_group = "ObsidianRightArrow" },
          ["!"] = { char = "", hl_group = "ObsidianImportant" },
          ["-"] = { char = "󰰱", hl_group = "ObsidianTilde" },
          ["?"] = { char = "", hl_group = "ObsidianQuestion" },
          [">"] = { char = "", hl_group = "ObsidianDeferred" },
          ["*"] = { char = "󰓎", hl_group = "ObsidianStar" },
        },
        hl_groups = {
          ObsidianTodo = { bold = true, fg = "#f78c6c" },
          ObsidianDone = { bold = true, fg = "#89ddff" },
          ObsidianRightArrow = { bold = true, fg = "#f78c6c" },
          ObsidianImportant = { bold = true, fg = "#e5484d" },
          ObsidianTilde = { bold = true, fg = "#737994" },
          ObsidianQuestion = { bold = true, fg = "#e5a700" },
          ObsidianDeferred = { bold = true, fg = "#6e56cf" },
          ObsidianStar = { bold = true, fg = "#e5a700" },
        },
      },
    },
  },
}
