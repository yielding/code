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
      if vim.fn.has("macunix") == 1 then
        require("dbee").setup()
      else
        require("dbee").setup({
          sources = {
            require("dbee.sources").FileSource:new("/home/yielding/claude/psql/dbee.json"),
          },
        })
      end
    end,
    keys = {
      { "<leader>De", function() require("dbee").toggle() end, desc = "Toggle DB Explorer" },
    },
  },
}
