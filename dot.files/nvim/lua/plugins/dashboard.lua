-- ~/.config/nvim/lua/plugins/dashboard.lua
return {
  "folke/snacks.nvim",
  opts = {
    dashboard = {
      sections = {
        { section = "header", padding = 2 },
        {
          section = "keys",
          padding = 1,
        },
        { section = "startup", padding = 1 },
      },
      preset = {
        keys = {
          { icon = " ", key = "f", desc = "Find File", action = ":Files" },
          { icon = " ", key = "n", desc = "New File", action = ":ene | startinsert" },
          { icon = " ", key = "g", desc = "Find Text", action = ":Rg" },
          { icon = " ", key = "r", desc = "Recent Files", action = ":FZF ~" },
          { icon = " ", key = "c", desc = "Config", action = ":e $MYVIMRC" },
          { icon = "󰒲 ", key = "l", desc = "Lazy", action = ":Lazy" },
          { icon = " ", key = "q", desc = "Quit", action = ":qa" },
        },
      },
    },
  },
}
