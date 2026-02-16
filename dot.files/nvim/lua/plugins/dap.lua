-- lua/plugins/dap.lua
-- Debug Adapter Protocol configuration
---@diagnostic disable: undefined-field, missing-fields, inject-field

return {
  -- nvim-dap: Debug Adapter Protocol client
  {
    "mfussenegger/nvim-dap",
    dependencies = {
      "nvim-neotest/nvim-nio",
    },
    config = function()
      local dap = require("dap")

      -- Highlight groups for DAP signs
      local function set_dap_highlights()
        vim.api.nvim_set_hl(0, "DapBreakpoint", { fg = "#ff0000", bold = true })  -- Red
        vim.api.nvim_set_hl(0, "DapBreakpointCondition", { fg = "#ffff00", bold = true })  -- Yellow
        vim.api.nvim_set_hl(0, "DapLogPoint", { fg = "#00bfff", bold = true })  -- Blue
        vim.api.nvim_set_hl(0, "DapStopped", { fg = "#00ff00", bold = true })  -- Green
        vim.api.nvim_set_hl(0, "DapStoppedLine", { bg = "#2e4d3d" })  -- Dark green bg
        vim.api.nvim_set_hl(0, "DapBreakpointRejected", { fg = "#888888" })  -- Gray
      end
      set_dap_highlights()
      -- Colorscheme 변경 시 다시 적용
      vim.api.nvim_create_autocmd("ColorScheme", {
        callback = set_dap_highlights,
      })

      -- Signs for breakpoints (Font Awesome / Codicons)
      vim.fn.sign_define("DapBreakpoint", { text = "\u{f111}", texthl = "DapBreakpoint", linehl = "", numhl = "" })  --
      vim.fn.sign_define("DapBreakpointCondition", { text = "\u{f059}", texthl = "DapBreakpointCondition", linehl = "", numhl = "" })  --
      vim.fn.sign_define("DapLogPoint", { text = "\u{f05a}", texthl = "DapLogPoint", linehl = "", numhl = "" })  --
      vim.fn.sign_define("DapStopped", { text = "\u{f04b}", texthl = "DapStopped", linehl = "DapStoppedLine", numhl = "" })  --
      vim.fn.sign_define("DapBreakpointRejected", { text = "\u{f05e}", texthl = "DapBreakpointRejected", linehl = "", numhl = "" })  --

      -- codelldb adapter (for C++, Rust)
      -- Try to find codelldb in common locations
      local codelldb_path = vim.fn.expand("~/.local/share/nvim/mason/bin/codelldb")
      if vim.fn.executable(codelldb_path) == 0 then
        codelldb_path = vim.fn.expand("~/.vscode/extensions/vadimcn.vscode-lldb-*/adapter/codelldb")
        local paths = vim.fn.glob(codelldb_path, false, true)
        if #paths > 0 then
          codelldb_path = paths[1]
        end
      end

      dap.adapters.codelldb = {
        type = "server",
        port = "${port}",
        executable = {
          command = codelldb_path,
          args = { "--port", "${port}" },
        },
      }

      -- C++ configuration
      dap.configurations.cpp = {
        {
          name = "Launch file",
          type = "codelldb",
          request = "launch",
          program = function()
            return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
          end,
          cwd = "${workspaceFolder}",
          stopOnEntry = false,
          args = {},
        },
        {
          name = "Launch with args",
          type = "codelldb",
          request = "launch",
          program = function()
            return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
          end,
          cwd = "${workspaceFolder}",
          stopOnEntry = false,
          args = function()
            local args_string = vim.fn.input("Arguments: ")
            return vim.split(args_string, " ")
          end,
        },
      }

      -- Rust uses same config as C++
      dap.configurations.rust = dap.configurations.cpp
      dap.configurations.c = dap.configurations.cpp

      -- Python configuration (debugpy)
      dap.adapters.python = function(cb, config)
        if config.request == "attach" then
          local port = (config.connect or config).port
          local host = (config.connect or config).host or "127.0.0.1"
          cb({
            type = "server",
            port = assert(port, "`connect.port` is required for a python `attach` configuration"),
            host = host,
            options = {
              source_filetype = "python",
            },
          })
        else
          cb({
            type = "executable",
            command = vim.g.python3_host_prog or "python3",
            args = { "-m", "debugpy.adapter" },
            options = {
              source_filetype = "python",
            },
          })
        end
      end

      dap.configurations.python = {
        {
          type = "python",
          request = "launch",
          name = "Launch file",
          program = "${file}",
          pythonPath = function()
            return vim.g.python3_host_prog or "/opt/homebrew/bin/python3"
          end,
        },
      }

      dap.adapters.ruby = function(callback, config)
        callback({
          type = "executable",
          command = "rdbg",
          args = { "--open=vscode", "--nonstop", "--command", "--", config.program == "${file}" and vim.fn.expand("%:p") or config.program },
        })
      end


      dap.configurations.ruby = {
        {
          type = "ruby",
          name = "Debug current file",
          request = "launch",
          program = "${file}",
        },
      }

      -- Global keymaps for debugging
      vim.keymap.set("n", "<F5>", function()
        if dap.session() then
          dap.continue()
        else
          -- 자동으로 첫 번째 설정 실행
          local ft = vim.bo.filetype
          local configs = dap.configurations[ft]
          if configs and configs[1] then
            dap.run(configs[1])
          else
            dap.continue()
          end
        end
      end, { desc = "Debug: Start/Continue" })
      vim.keymap.set("n", "<F6>", dap.terminate, { desc = "Debug: Stop" })
      vim.keymap.set("n", "<F7>", dap.restart, { desc = "Debug: Restart" })
      vim.keymap.set("n", "<F8>", dap.pause, { desc = "Debug: Pause" })
      vim.keymap.set("n", "<F9>", dap.toggle_breakpoint, { desc = "Debug: Toggle Breakpoint" })
      vim.keymap.set("n", "<F10>", dap.step_over, { desc = "Debug: Step Over" })
      vim.keymap.set("n", "<F11>", dap.step_into, { desc = "Debug: Step Into" })
      vim.keymap.set("n", "<F12>", dap.step_out, { desc = "Debug: Step Out" })

      -- Additional debug keymaps
      vim.keymap.set("n", "<leader>db", dap.toggle_breakpoint, { desc = "Debug: Toggle Breakpoint" })
      vim.keymap.set("n", "<leader>dB", function()
        dap.set_breakpoint(vim.fn.input("Breakpoint condition: "))
      end, { desc = "Debug: Conditional Breakpoint" })
      vim.keymap.set("n", "<leader>dr", dap.repl.open, { desc = "Debug: Open REPL" })
      vim.keymap.set("n", "<leader>dl", dap.run_last, { desc = "Debug: Run Last" })
      vim.keymap.set("n", "<leader>df", dap.terminate, { desc = "Debug: Finish/Reset" })

      -- Hover evaluation
      vim.keymap.set({ "n", "v" }, "<leader>di", function()
        require("dap.ui.widgets").hover()
      end, { desc = "Debug: Inspect" })
    end,
  },

  -- nvim-dap-ui: UI for nvim-dap
  {
    "rcarriga/nvim-dap-ui",
    dependencies = {
      "mfussenegger/nvim-dap",
      "nvim-neotest/nvim-nio",
    },
    config = function()
      local dap = require("dap")
      local dapui = require("dapui")

      dapui.setup({
        icons = { expanded = "▾", collapsed = "▸", current_frame = "→" },
        controls = {
          icons = {
            pause = "󰏤",
            play = "󰐊",
            step_into = "󰆹",
            step_over = "󰆷",
            step_out = "󰆸",
            step_back = "󰌍",
            run_last = "󰑐",
            terminate = "󰓛",
            disconnect = "󰈂",
          },
        },
        layouts = {
          {
            elements = {
              { id = "scopes", size = 0.4 },
              { id = "breakpoints", size = 0.2 },
              { id = "stacks", size = 0.2 },
              { id = "watches", size = 0.2 },
            },
            position = "left",
            size = 40,
          },
          {
            elements = {
              { id = "repl", size = 0.5 },
              { id = "console", size = 0.5 },
            },
            position = "bottom",
            size = 10,
          },
        },
      })

      -- Auto open/close dapui
      dap.listeners.after.event_initialized["dapui_config"] = function()
        dapui.open()
      end
      dap.listeners.before.event_terminated["dapui_config"] = function()
        dapui.close()
      end
      dap.listeners.before.event_exited["dapui_config"] = function()
        dapui.close()
      end

      -- Toggle dapui
      vim.keymap.set("n", "<leader>du", dapui.toggle, { desc = "Debug: Toggle UI" })
    end,
  },

  -- Mason-nvim-dap: Auto-install debug adapters
  {
    "jay-babu/mason-nvim-dap.nvim",
    dependencies = {
      "williamboman/mason.nvim",
      "mfussenegger/nvim-dap",
    },
    config = function()
      require("mason-nvim-dap").setup({
        ensure_installed = { "codelldb", "debugpy" },
        automatic_installation = true,
        handlers = {},
      })
    end,
  },

  -- nvim-dap-virtual-text: Show variable values inline
  {
    "theHamsta/nvim-dap-virtual-text",
    dependencies = {
      "mfussenegger/nvim-dap",
      "nvim-treesitter/nvim-treesitter",
    },
    config = function()
      require("nvim-dap-virtual-text").setup({
        enabled = true,
        enabled_commands = true,
        highlight_changed_variables = true,
        highlight_new_as_changed = false,
        show_stop_reason = true,
        commented = false,
        virt_text_pos = "eol",
        all_frames = false,
        virt_lines = false,
        virt_text_win_col = nil,
      })
    end,
  },
}
