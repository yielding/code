-- lua/plugins/telescope.lua
return {
  "nvim-telescope/telescope.nvim",
  branch = "master",
  dependencies = {
    "nvim-lua/plenary.nvim",
    { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
  },
  cmd = "Telescope",
  keys = {
    { "<leader>ff", "<cmd>Telescope find_files<CR>", desc = "Telescope find files" },
    { "<leader>rg", "<cmd>Telescope live_grep<CR>", desc = "Telescope live grep" },
    { "<leader>bf", "<cmd>Telescope buffers<CR>", desc = "Telescope buffers" },
    { "<leader>gf", "<cmd>Telescope git_files<CR>", desc = "Telescope git files" },
    { "<leader>fh", "<cmd>Telescope help_tags<CR>", desc = "Telescope help tags" },
    { "<leader>fs", "<cmd>Telescope grep_string<CR>", desc = "Telescope grep string" },
    { "<leader>fd", "<cmd>Telescope diagnostics<CR>", desc = "Telescope diagnostics" },
    { "<leader>fr", "<cmd>Telescope resume<CR>", desc = "Telescope resume" },
  },
  config = function()
    local telescope = require("telescope")
    telescope.setup({
      defaults = {
        layout_strategy = "horizontal",
        layout_config = {
          horizontal = { preview_width = 0.55 },
          width = 0.87,
          height = 0.80,
        },
        mappings = {
          i = {
            ["<C-j>"] = "move_selection_next",
            ["<C-k>"] = "move_selection_previous",
            ["<Esc>"] = "close",
          },
        },
      },
      extensions = {
        fzf = {
          fuzzy = true,
          override_generic_sorter = true,
          override_file_sorter = true,
        },
      },
    })
    telescope.load_extension("fzf")
  end,
}
