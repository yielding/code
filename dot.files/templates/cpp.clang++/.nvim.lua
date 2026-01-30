-- Project-specific nvim-dap configuration
local ok, dap = pcall(require, "dap")
if not ok then return end

dap.configurations.cpp = {
  {
    name = "Launch main",
    type = "codelldb",
    request = "launch",
    program = "${workspaceFolder}/build/debug/main",
    cwd = "${workspaceFolder}",
    stopOnEntry = false,
    args = {},
  },
  {
    name = "Launch with args",
    type = "codelldb",
    request = "launch",
    program = "${workspaceFolder}/build/debug/main",
    cwd = "${workspaceFolder}",
    stopOnEntry = false,
    args = function()
      local args_string = vim.fn.input("Arguments: ")
      return vim.split(args_string, " ")
    end,
  },
}
