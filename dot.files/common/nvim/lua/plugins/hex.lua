-- lua/plugins/hex.lua
-- Hex editing for binary files

return {
  {
    "RaafatTurki/hex.nvim",
    cmd = { "HexDump", "HexAssemble", "HexToggle" },
    opts = {},
    keys = {
      { "<leader>hx", "<cmd>HexToggle<cr>", desc = "Toggle Hex View" },
    },
  },
}
