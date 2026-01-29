-- lua/plugins/init.lua
-- lazy.nvim plugin specifications

return {
  -- Import plugin modules
  { import = "plugins.lsp" },
  { import = "plugins.cmp" },
  { import = "plugins.dap" },
  { import = "plugins.treesitter" },
  { import = "plugins.ui" },
  { import = "plugins.git" },
  { import = "plugins.editor" },
  { import = "plugins.tools" },
}
