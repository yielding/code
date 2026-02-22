-- lua/config/keymaps.lua
-- Key mappings

local map = vim.keymap.set
local opts = { noremap = true, silent = true }

-- Helper for global variables in mappings
local function expand_var(var)
  return vim.fn.expand(vim.g[var] or "")
end

-- Basic shortcuts
map("n", "<leader>f", "[I", { noremap = true, silent = true, desc = "Show word occurrences" })
map("n", "<leader>l", ":set list!<CR>", { noremap = true, silent = true, desc = "Toggle listchars" })
map("n", "<leader>n", ":set nu!<CR>", { noremap = true, silent = true, desc = "Toggle line numbers" })
map("n", "<leader>w", ":w<CR>", { noremap = true, silent = true, desc = "Save file" })
map("n", "<leader>q", ":q<CR>", { noremap = true, silent = true, desc = "Quit" })
map({ "n", "i" }, "<C-S-s>", "<Cmd>w<CR>", opts)
map("n", "<leader>cd", ":lcd %:p:h<CR>:pwd<CR>", { noremap = true, silent = true, desc = "LCD to buffer dir" })

-- File editing shortcuts
map("n", "<leader>v", function() vim.cmd("edit " .. expand_var("_vimrc")) end, { noremap = true, silent = true, desc = "Edit vimrc" })
map("n", "<leader>p", function() vim.cmd("edit " .. expand_var("_plugs")) end, { noremap = true, silent = true, desc = "Edit plugins" })
map("n", "<leader>z", function() vim.cmd("edit " .. expand_var("_zshrc")) end, { noremap = true, silent = true, desc = "Edit zshrc" })
map("n", "<leader>x", function() vim.cmd("edit " .. expand_var("_tmuxrc")) end, { noremap = true, silent = true, desc = "Edit tmux.conf" })
map("n", "<leader>u", function() vim.cmd("source " .. expand_var("_vimrc")) end, { noremap = true, silent = true, desc = "Source vimrc" })

-- Escaping
map("i", "jk", "<Esc>", opts)
map("c", "jk", "<C-c>", opts)

-- Visual enclose with parenthesis
map("v", "<leader>ss", 'c()<Esc>P', { noremap = true, silent = true, desc = "Surround with ()" })

-- Movement in insert mode
map("i", "<A-h>", "<C-o>h", opts)
map("i", "<A-l>", "<C-o>a", opts)
map("i", "<A-j>", "<C-o>j", opts)
map("i", "<A-k>", "<C-o>k", opts)

-- qq to record, Q to replay
map("n", "Q", "@q", opts)

-- Snippet editing
map("n", "<leader>ec", function() vim.cmd("edit " .. expand_var("_mysnippet") .. "cpp.lua") end, { noremap = true, silent = true, desc = "Edit C++ snippets" })
map("n", "<leader>ep", function() vim.cmd("edit " .. expand_var("_mysnippet") .. "python.lua") end, { noremap = true, silent = true, desc = "Edit Python snippets" })
map("n", "<leader>er", function() vim.cmd("edit " .. expand_var("_mysnippet") .. "ruby.lua") end, { noremap = true, silent = true, desc = "Edit Ruby snippets" })
map("n", "<leader>em", function() vim.cmd("edit " .. expand_var("_mysnippet") .. "cmake.lua") end, { noremap = true, silent = true, desc = "Edit CMake snippets" })

-- Test files
map("n", "<leader>tc", function()
  vim.cmd("lcd " .. expand_var("_test"))
  vim.cmd("edit " .. expand_var("_test") .. "test.cpp")
end, { noremap = true, silent = true, desc = "Edit test.cpp" })
map("n", "<leader>tr", function() vim.cmd("edit " .. expand_var("_test") .. "test.rb") end, { noremap = true, silent = true, desc = "Edit test.rb" })
map("n", "<leader>tp", function() vim.cmd("edit " .. expand_var("_test") .. "test.py") end, { noremap = true, silent = true, desc = "Edit test.py" })
map("n", "<leader>tm", function() vim.cmd("edit " .. expand_var("_test") .. "test.md") end, { noremap = true, silent = true, desc = "Edit test.md" })

-- Remove ^M characters
map("n", "<leader>kk", [[:%s/<C-V><CR>//ge<CR>:w<CR>]], { noremap = true, silent = true, desc = "Remove ^M characters" })

-- TagBar
map("n", "<leader>9", ":TagbarToggle<CR>", { noremap = true, silent = true, desc = "Toggle Tagbar" })

-- CMake keymaps are in lua/plugins/tools.lua (buffer-local for cpp/c/cmake)

-- Grep in git dir
map("n", "<leader>gc", ":silent grep <cword><CR>", { noremap = true, silent = true, desc = "Grep word under cursor" })
map("n", "<leader>gC", ":silent grep <cWORD><CR>", { noremap = true, silent = true, desc = "Grep WORD under cursor" })

-- Buffers
map("n", "]b", ":bnext<CR>", opts)
map("n", "[b", ":bprev<CR>", opts)
map("n", "<leader>cd", ":cd %:p:h<CR>:pwd<CR>", { noremap = true, silent = true, desc = "CD to buffer dir" })

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
map("n", "<leader>s", toggle_hlsearch, { noremap = true, silent = true, desc = "Toggle hlsearch" })

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
map("n", "<leader>S", toggle_syntax, { noremap = true, silent = true, desc = "Toggle syntax" })

-- Toggle transparent background
local transparent_bg = true

local function set_transparent()
  vim.api.nvim_set_hl(0, "Normal", { bg = "NONE" })
  vim.api.nvim_set_hl(0, "NormalNC", { bg = "NONE" })
  vim.api.nvim_set_hl(0, "NormalFloat", { bg = "NONE" })
  vim.api.nvim_set_hl(0, "NvimTreeNormal", { bg = "NONE" })
  vim.api.nvim_set_hl(0, "NvimTreeNormalNC", { bg = "NONE" })
  vim.api.nvim_set_hl(0, "NvimTreeWinSeparator", { fg = "NONE", bg = "NONE" })
  vim.api.nvim_set_hl(0, "WinSeparator", { fg = "NONE", bg = "NONE" })
  vim.api.nvim_set_hl(0, "SignColumn", { bg = "NONE" })
  vim.api.nvim_set_hl(0, "NvimTreeEndOfBuffer", { bg = "NONE" })
  vim.opt.fillchars:append({ vert = " " })
end

local function set_opaque()
  local colors = require("catppuccin.palettes").get_palette()
  vim.api.nvim_set_hl(0, "Normal", { fg = colors.text, bg = colors.base })
  vim.api.nvim_set_hl(0, "NormalNC", { fg = colors.text, bg = colors.base })
  vim.api.nvim_set_hl(0, "NormalFloat", { fg = colors.text, bg = colors.mantle })
  vim.api.nvim_set_hl(0, "NvimTreeNormal", { fg = colors.text, bg = colors.mantle })
  vim.api.nvim_set_hl(0, "NvimTreeNormalNC", { fg = colors.text, bg = colors.mantle })
  vim.api.nvim_set_hl(0, "NvimTreeWinSeparator", { fg = colors.mantle, bg = colors.mantle })
  vim.api.nvim_set_hl(0, "WinSeparator", { fg = colors.surface0, bg = colors.base })
  vim.api.nvim_set_hl(0, "SignColumn", { bg = colors.base })
  vim.api.nvim_set_hl(0, "NvimTreeEndOfBuffer", { fg = colors.mantle, bg = colors.mantle })
  vim.opt.fillchars:append({ vert = "┃" })
end

local function toggle_transparent_bg()
  transparent_bg = not transparent_bg
  if transparent_bg then
    set_transparent()
    print("@ Background : Transparent")
  else
    set_opaque()
    print("@ Background : Opaque")
  end
end

-- Apply transparent on startup after colorscheme loads
vim.api.nvim_create_autocmd("ColorScheme", {
  callback = function()
    if transparent_bg then set_transparent() end
  end,
})
vim.defer_fn(set_transparent, 0)

map("n", "<leader>bg", toggle_transparent_bg, { noremap = true, silent = true, desc = "Toggle transparent background" })

-- Run current file
local run_cmd = {
  python   = "python3",
  ruby     = "ruby --jit",
  go       = "go run",
  lua      = "lua",
  bash     = "bash",
  zsh      = "zsh",
  sh       = "sh",
  javascript = "node",
  typescript = "npx ts-node",
  rust     = "cargo run",
  kotlin   = "kotlinc -script",
  haskell  = "runhaskell",
}

map("n", "<leader>rr", function()
  local src = vim.fn.tempname()
  vim.cmd("silent write " .. src)
  local ft = vim.bo.filetype
  local cmd = run_cmd[ft]
  if not cmd then
    vim.notify("No run command for filetype: " .. ft, vim.log.levels.WARN)
    return
  end
  vim.cmd("below pedit! Run\\ Output")
  vim.cmd("wincmd P")
  vim.bo.buftype = "nofile"
  vim.bo.swapfile = false
  vim.bo.syntax = ""
  vim.bo.bufhidden = "delete"
  vim.cmd("silent %!" .. cmd .. " " .. src .. " 2>&1")
  vim.cmd("wincmd p")
end, { noremap = true, silent = true, desc = "Run current file" })

-- JQ command for JSON formatting
vim.api.nvim_create_user_command("JQ", "%!jq .", {})

-- Switch header/source via clangd
local function switch_source_header()
  local params = { uri = vim.uri_from_bufnr(0) }
  vim.lsp.buf_request(0, "textDocument/switchSourceHeader", params, function(err, result)
    if err or not result or result == "" then return end
    vim.cmd("edit " .. vim.uri_to_fname(result))
  end)
end

map("n", "<leader>a", switch_source_header, { noremap = true, silent = true, desc = "Switch header/source" })
vim.api.nvim_create_user_command("A", switch_source_header, {})
