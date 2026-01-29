-- lua/plugins/editor.lua
-- Editor enhancement plugins

return {
  -- auto-pairs: Auto close brackets
  {
    "jiangmiao/auto-pairs",
  },

  -- vim-projectionist: Project configuration
  {
    "tpope/vim-projectionist",
  },

  -- vim-abolish: Smart substitution
  {
    "tpope/vim-abolish",
  },

  -- easymotion: Fast cursor movement
  {
    "easymotion/vim-easymotion",
    config = function()
      -- Default mappings are fine
    end,
  },

  -- closetag: Auto close HTML tags
  {
    "vim-scripts/closetag.vim",
    ft = { "html", "xml", "xhtml", "phtml", "jsx", "tsx" },
  },

  -- matchit: Extended % matching
  {
    "vim-scripts/matchit.zip",
  },

  -- plenary: Lua utilities (required by many plugins)
  {
    "nvim-lua/plenary.nvim",
  },

  -- markdown-preview
  {
    "iamcco/markdown-preview.nvim",
    cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
    ft = { "markdown" },
    build = "cd app && yarn install",
    init = function()
      vim.g.mkdp_filetypes = { "markdown" }
    end,
  },
}
