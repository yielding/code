-- lua/utils/helpers.lua
-- Utility functions

local M = {}

--- Check if a plugin is loaded
---@param name string Plugin name
---@return boolean
function M.is_loaded(name)
  return package.loaded[name] ~= nil
end

--- Safe require with error handling
---@param module string Module name
---@return any|nil
function M.safe_require(module)
  local ok, result = pcall(require, module)
  if ok then
    return result
  end
  vim.notify("Failed to load module: " .. module, vim.log.levels.WARN)
  return nil
end

--- Create a keymap with default options
---@param mode string|table Mode(s)
---@param lhs string Left-hand side
---@param rhs string|function Right-hand side
---@param opts table|nil Additional options
function M.map(mode, lhs, rhs, opts)
  opts = vim.tbl_extend("force", { noremap = true, silent = true }, opts or {})
  vim.keymap.set(mode, lhs, rhs, opts)
end

--- Create an augroup with autocommands
---@param name string Augroup name
---@param autocmds table List of autocmd definitions
function M.augroup(name, autocmds)
  local group = vim.api.nvim_create_augroup(name, { clear = true })
  for _, autocmd in ipairs(autocmds) do
    autocmd.group = group
    vim.api.nvim_create_autocmd(autocmd[1], {
      group = group,
      pattern = autocmd.pattern,
      callback = autocmd.callback,
      command = autocmd.command,
    })
  end
end

--- Toggle an option
---@param option string Option name
---@param values table|nil Optional values to cycle through
function M.toggle_option(option, values)
  if values then
    local current = vim.opt[option]:get()
    local idx = 1
    for i, v in ipairs(values) do
      if v == current then
        idx = i
        break
      end
    end
    local next_idx = (idx % #values) + 1
    vim.opt[option] = values[next_idx]
    vim.notify(option .. " = " .. tostring(values[next_idx]))
  else
    vim.opt[option] = not vim.opt[option]:get()
    vim.notify(option .. " = " .. tostring(vim.opt[option]:get()))
  end
end

--- Get visual selection
---@return string
function M.get_visual_selection()
  local s_start = vim.fn.getpos("'<")
  local s_end = vim.fn.getpos("'>")
  local n_lines = math.abs(s_end[2] - s_start[2]) + 1
  local lines = vim.api.nvim_buf_get_lines(0, s_start[2] - 1, s_end[2], false)
  lines[1] = string.sub(lines[1], s_start[3], -1)
  if n_lines == 1 then
    lines[n_lines] = string.sub(lines[n_lines], 1, s_end[3] - s_start[3] + 1)
  else
    lines[n_lines] = string.sub(lines[n_lines], 1, s_end[3])
  end
  return table.concat(lines, "\n")
end

return M
