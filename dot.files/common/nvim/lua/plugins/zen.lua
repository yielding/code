-- lua/plugins/zen.lua
-- Zen mode and Twilight for distraction-free editing

return {
  -- twilight: Dim inactive code
  {
    "folke/twilight.nvim",
    cmd = { "Twilight", "TwilightEnable", "TwilightDisable" },
    opts = {
      dimming = {
        alpha = 0.25,
      },
      context = 10,
      treesitter = true,
    },
    keys = {
      { "<leader>zt", "<cmd>Twilight<cr>", desc = "Toggle Twilight" },
    },
  },

  -- zen-mode: Distraction-free editing
  {
    "folke/zen-mode.nvim",
    cmd = { "ZenMode" },
    dependencies = { "folke/twilight.nvim" },
    opts = {
      window = {
        width = 120,
        options = {
          signcolumn = "no",
          number = false,
          relativenumber = false,
          cursorline = false,
        },
      },
      plugins = {
        twilight = { enabled = true },
        gitsigns = { enabled = false },
      },
    },
    keys = {
      { "<leader>zz", "<cmd>ZenMode<cr>", desc = "Toggle Zen Mode" },
    },
  },
}
