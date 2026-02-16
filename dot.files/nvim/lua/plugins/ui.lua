-- lua/plugins/ui.lua
-- UI plugins: colorschemes, statusline, file explorer
---@diagnostic disable: missing-fields

return {
  -- Colorschemes
  {
    "EdenEast/nightfox.nvim",
    lazy = false,
    priority = 1000,
  },

  {
    "morhetz/gruvbox",
    lazy = true,
  },

  {
    "folke/tokyonight.nvim",
    opts = {
      style = "night",
    },
  },

  {
    "catppuccin/nvim",
    name = "catppuccin",
    lazy = false,
    priority = 1000,
    opts = {
      flavour = "mocha",
    },
  },

  -- Lualine: Statusline
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      ---@diagnostic disable-next-line: missing-fields
      require("lualine").setup({
        options = {
          theme = "wombat",
          component_separators = { left = "", right = "" },
          section_separators = { left = "", right = "" },
          globalstatus = true,
        },
        sections = {
          lualine_a = { "mode" },
          lualine_b = { "branch", "diff", "diagnostics" },
          lualine_c = { { "filename", path = 1 } },
          lualine_x = { "encoding", { "fileformat", symbols = { unix = "", dos = "", mac = "" } }, "filetype" },
          lualine_y = { "progress" },
          lualine_z = { "location" },
        },
      })
    end,
  },

  -- nvim-tree: File explorer
  {
    "nvim-tree/nvim-tree.lua",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("nvim-tree").setup({
        sync_root_with_cwd = true,   -- ,cd 명령 시 동기화
        respect_buf_cwd = false,     -- 버퍼의 lcd 무시
        update_focused_file = {
          enable = false,            -- 버퍼 변경 시 자동 갱신 안함
          update_root = false,
        },
        sort = {
          sorter = "case_sensitive",
        },
        view = {
          width = 30,
          side = "left",
        },
        renderer = {
          group_empty = true,
          icons = {
            show = {
              file = true,
              folder = true,
              folder_arrow = true,
              git = true,
            },
          },
        },
        filters = {
          dotfiles = false,
        },
        git = {
          enable = true,
          ignore = false,
        },
        actions = {
          open_file = {
            quit_on_open = false,
          },
        },
      })

      -- Keymaps
      vim.keymap.set("n", "<leader>ee", ":NvimTreeToggle<CR>", { silent = true })
      vim.keymap.set("n", "<leader>ef", ":NvimTreeFindFile<CR>", { silent = true })

      -- Auto open nvim-tree when opening a directory
      vim.api.nvim_create_autocmd("VimEnter", {
        callback = function(data)
          if vim.fn.isdirectory(data.file) == 1 then
            vim.cmd.cd(data.file)
            require("nvim-tree.api").tree.open()
          end
        end,
      })
    end,
  },

  -- nvim-web-devicons
  {
    "nvim-tree/nvim-web-devicons",
    config = function()
      require("nvim-web-devicons").setup({
        default = true,
        override = {
          rs = {
            icon = "󱘗",
            color = "#dea584",
            name = "Rust",
          },
        },
        override_by_filename = {
          [".tmux.conf"] = {
            icon = "󰆍",
            color = "#1bb91f",
            name = "Tmux",
          },
        },
      })
    end,
  },

  -- quicker.nvim: Better quickfix
  { "stevearc/quicker.nvim",
    event = "FileType qf",
    config = function()
      require("quicker").setup()
    end,
  },

  -- zen-mode.nvim: Distraction-free writing
  {
    "folke/zen-mode.nvim",
    cmd = "ZenMode",
    keys = {
      { "<leader>zz", "<cmd>ZenMode<CR>", desc = "Zen Mode" },
    },
    opts = {
      window = {
        width = 90,
        options = {
          number = false,
          relativenumber = false,
          signcolumn = "no",
          cursorline = false,
        },
      },
      plugins = {
        gitsigns = { enabled = false },
        tmux = { enabled = true },
        twilight = { enabled = false },
      },
    },
  },

  -- twilight.nvim: Dim inactive code
  {
    "folke/twilight.nvim",
    cmd = { "Twilight", "TwilightEnable", "TwilightDisable" },
    keys = {
      { "<leader>zt", "<cmd>Twilight<CR>", desc = "Twilight" },
    },
    opts = {},
  },

  -- noice.nvim: Better UI for cmdline, messages, notifications
  {
    "folke/noice.nvim",
    event = "VeryLazy",
    dependencies = {
      "MunifTanjim/nui.nvim",
      "rcarriga/nvim-notify",
    },
    config = function()
      require("noice").setup({
        cmdline = {
          enabled = true,
          view = "cmdline_popup",
          opts = {
            win_options = {
              winhighlight = "Normal:NormalFloat,FloatBorder:FloatBorder",
            },
          },
          format = {
            cmdline = { pattern = "^:", icon = ">", lang = "" },
            search_down = { kind = "search", pattern = "^/", icon = " ", lang = "" },
            search_up = { kind = "search", pattern = "^%?", icon = " ", lang = "" },
            filter = { pattern = "^:%s*!", icon = "$", lang = "" },
            lua = { pattern = { "^:%s*lua%s+", "^:%s*lua%s*=%s*", "^:%s*=%s*" }, icon = "", lang = "" },
            help = { pattern = "^:%s*he?l?p?%s+", icon = "?", lang = "" },
          },
        },
        messages = {
          enabled = true,
          view = "notify",
        },
        popupmenu = {
          enabled = true,
          backend = "nui",
        },
        lsp = {
          progress = {
            enabled = true,
          },
          override = {
            ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
            ["vim.lsp.util.stylize_markdown"] = true,
            ["cmp.entry.get_documentation"] = true,
          },
        },
        presets = {
          bottom_search = false,
          command_palette = true,
          long_message_to_split = true,
          lsp_doc_border = true,
        },
        -- 오류 방지
        routes = {
          {
            filter = {
              event = "msg_show",
              kind = "",
              find = "written",
            },
            opts = { skip = true },
          },
        },
      })
    end,
  },

 -- which-key: Display keybindings in popup
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
      preset = "modern",
      delay = 300,
    },
    keys = {
      { "<leader>?", function() require("which-key").show({ global = false }) end, desc = "Buffer Local Keymaps" },
    },
  },

}
