-- lua/plugins/dbee.lua
-- Database client for Neovim

return {
  {
    "kndndrj/nvim-dbee",
    dependencies = {
      "MunifTanjim/nui.nvim",
    },
    build = function()
      require("dbee").install()
    end,
    cmd = { "Dbee" },
    config = function()
      require("dbee").setup({
        sources = {
          require("dbee.sources").FileSource:new("/home/yielding/claude/psql/dbee.json"),
        },
      })
    end,
    keys = {
      { "<leader>De", function() require("dbee").toggle() end, desc = "Toggle DB Explorer" },
    },
  },
}
