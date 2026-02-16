-- lua/config/keymaps.lua
-- Key mappings

local map = vim.keymap.set
local opts = { noremap = true, silent = true }

-- Helper for global variables in mappings
local function expand_var(var)
  return vim.fn.expand(vim.g[var] or "")
end

-- Basic shortcuts
map("n", "<leader>f", "[I", opts)
map("n", "<leader>l", ":set list!<CR>", opts)
map("n", "<leader>n", ":set nu!<CR>", opts)
map("n", "<leader>w", ":w<CR>", opts)
map("n", "<leader>q", ":q<CR>", opts)
map({ "n", "i" }, "<C-S-s>", "<Cmd>w<CR>", opts)
map("n", "<leader>cd", ":lcd %:p:h<CR>:pwd<CR>", opts)  -- lcd to current buffer's folder

-- File editing shortcuts
map("n", "<leader>v", function() vim.cmd("edit " .. expand_var("_vimrc")) end, opts)
map("n", "<leader>p", function() vim.cmd("edit " .. expand_var("_plugs")) end, opts)
map("n", "<leader>z", function() vim.cmd("edit " .. expand_var("_zshrc")) end, opts)
map("n", "<leader>x", function() vim.cmd("edit " .. expand_var("_tmuxrc")) end, opts)
map("n", "<leader>u", function() vim.cmd("source " .. expand_var("_vimrc")) end, opts)

-- Escaping
map("i", "jk", "<Esc>", opts)
map("c", "jk", "<C-c>", opts)

-- Visual enclose with parenthesis
map("v", "<leader>ss", 'c()<Esc>P', opts)

-- Movement in insert mode
map("i", "<A-h>", "<C-o>h", opts)
map("i", "<A-l>", "<C-o>a", opts)
map("i", "<A-j>", "<C-o>j", opts)
map("i", "<A-k>", "<C-o>k", opts)

-- qq to record, Q to replay
map("n", "Q", "@q", opts)

-- Snippet editing
map("n", "<leader>ec", function() vim.cmd("edit " .. expand_var("_mysnippet") .. "cpp.lua") end, opts)
map("n", "<leader>ep", function() vim.cmd("edit " .. expand_var("_mysnippet") .. "python.lua") end, opts)
map("n", "<leader>er", function() vim.cmd("edit " .. expand_var("_mysnippet") .. "ruby.lua") end, opts)
map("n", "<leader>em", function() vim.cmd("edit " .. expand_var("_mysnippet") .. "cmake.lua") end, opts)

-- Test files
map("n", "<leader>tc", function()
  vim.cmd("lcd " .. expand_var("_test"))
  vim.cmd("edit " .. expand_var("_test") .. "test.cpp")
end, opts)
map("n", "<leader>tr", function() vim.cmd("edit " .. expand_var("_test") .. "test.rb") end, opts)
map("n", "<leader>tp", function() vim.cmd("edit " .. expand_var("_test") .. "test.py") end, opts)
map("n", "<leader>tm", function() vim.cmd("edit " .. expand_var("_test") .. "test.md") end, opts)

-- Remove ^M characters
map("n", "<leader>kk", [[:%s/<C-V><CR>//ge<CR>:w<CR>]], opts)

-- TagBar
map("n", "<leader>9", ":TagbarToggle<CR>", opts)

-- CMake keymaps are in lua/plugins/tools.lua (buffer-local for cpp/c/cmake)

-- Grep in git dir
map("n", "<leader>gc", ":silent grep <cword><CR>", opts)
map("n", "<leader>gC", ":silent grep <cWORD><CR>", opts)

-- Buffers
map("n", "]b", ":bnext<CR>", opts)
map("n", "[b", ":bprev<CR>", opts)
map("n", "<leader>cd", ":cd %:p:h<CR>:pwd<CR>", opts)

-- Tabs
map("n", "]t", ":tabn<CR>", opts)
map("n", "[t", ":tabp<CR>", opts)

-- Window navigation
map("n", "<C-h>", "<C-w>h", opts)
map("n", "<C-j>", "<C-w>j", opts)
map("n", "<C-k>", "<C-w>k", opts)
map("n", "<C-l>", "<C-w>l", opts)

-- Terminal escape
map("t", "<Esc>", [[<C-\><C-n>]], opts)
map("t", "<M-[>", "<Esc>", opts)
map("t", "<C-v><Esc>", "<Esc>", opts)

-- Terminal split
map("n", "<C-S-P>", ":split | resize 10 | terminal<CR>", opts)

-- Toggle fold
local function toggle_fold()
  if vim.fn.foldlevel(".") == 0 then
    vim.cmd("normal! l")
  else
    if vim.fn.foldclosed(".") < 0 then
      vim.cmd(". foldclose")
    else
      vim.cmd(". foldopen")
    end
  end
end
map("n", "<Space>", toggle_fold, opts)

-- Toggle hlsearch
local hlsearch_on = false
local function toggle_hlsearch()
  hlsearch_on = not hlsearch_on
  vim.opt.hlsearch = hlsearch_on
  if hlsearch_on then
    print("@ Switch HlSearch : ON")
  else
    print("@ Switch HlSearch : OFF")
  end
end
map("n", "<leader>s", toggle_hlsearch, opts)

-- Toggle syntax
local syntax_on = true
local function toggle_syntax()
  syntax_on = not syntax_on
  if syntax_on then
    vim.cmd("syntax on")
    print("@ Switch Syntax : ON")
  else
    vim.cmd("syntax off")
    print("@ Switch Syntax : OFF")
  end
end
map("n", "<leader>S", toggle_syntax, opts)

-- JQ command for JSON formatting
vim.api.nvim_create_user_command("JQ", "%!jq .", {})
