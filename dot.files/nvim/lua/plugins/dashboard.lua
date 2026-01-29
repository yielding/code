-- ~/.config/nvim/lua/plugins/dashboard.lua
return {
  "folke/snacks.nvim",
  priority = 1000,
  config = function(_, opts)
    require("snacks").setup(opts)
    -- Disable health checks for unsupported/unused modules
    local disabled = {
      "image", "dashboard", "notifier", "bigfile", "explorer",
      "input", "picker", "quickfile", "scope", "scroll",
      "statuscolumn", "toggle", "words",
    }
    for _, mod in ipairs(disabled) do
      local ok, m = pcall(require, "snacks." .. mod)
      if ok and m.meta then
        m.meta.health = false
      end
    end
  end,
  opts = {
    image = { enabled = false },
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
