-- ~/.config/nvim/init.lua
-- Neovim Lua Configuration Entry Point

-- Leader key must be set before lazy.nvim
vim.g.mapleader = ","
vim.g.maplocalleader = ","

-- Load core configuration
require("config.globals")
require("config.options")
require("config.autocmds")

-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable",
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

-- Load plugins
require("lazy").setup("plugins", {
  defaults = {
    lazy = false,
  },
  install = {
    colorscheme = { "darkbone", "duskfox", "habamax" },
  },
  checker = {
    enabled = false,
  },
  ui = {
    icons = {
      cmd = "⌘",
      config = "🛠",
      event = "📅",
      ft = "📂",
      init = "⚙",
      keys = "🗝",
      plugin = "🔌",
      runtime = "💻",
      require = "🌙",
      source = "📄",
      start = "🚀",
      task = "📌",
      lazy = "💤 ",
      loaded = "●",
      not_loaded = "○",
    },
  },
  performance = {
    rtp = {
      disabled_plugins = {
        "netrwPlugin",
        "netrw",
      },
    },
  },
})

-- Load keymaps after plugins
require("config.keymaps")

-- Set colorscheme
-- vim.cmd("colorscheme catppuccin-latte")
-- vim.cmd("colorscheme catppuccin-frappe")
-- vim.cmd("colorscheme catppuccin-mocha")
vim.cmd("colorscheme nordfox")
