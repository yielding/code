-- lua/config/options.lua
-- Vim options

local opt = vim.opt

-- General
opt.modeline = true
opt.ruler = true
opt.relativenumber = true
opt.autoread = true
opt.shiftround = true
opt.number = true
opt.numberwidth = 2
opt.endofline = false
opt.fixendofline = false
opt.laststatus = 2
opt.report = 0
opt.wildignore = { "*.obj", "*.o", "*.bak" }
opt.incsearch = true
opt.wrap = false
opt.commentstring = "//%s"
opt.showmatch = false
opt.backup = false
opt.updatetime = 300
opt.shortmess:append("c")
opt.clipboard = "unnamedplus"
opt.mouse = "a"
opt.timeout = false
opt.ttimeout = true
opt.hlsearch = false

-- Text, Tab and Indent
opt.autoindent = true
opt.autowrite = true
opt.smartindent = true
opt.virtualedit = "block"
opt.expandtab = true
opt.tabstop = 2
opt.softtabstop = 2
opt.shiftwidth = 2
opt.textwidth = 0
opt.smarttab = true
opt.cindent = true
opt.cinoptions = "t0,g0"
opt.fillchars:append({ vert = "┃" })
opt.splitbelow = true

-- Appearance
opt.termguicolors = true
opt.signcolumn = "yes"  -- 항상 sign column 표시 (브레이크포인트용)

-- Encodings
opt.fileencodings = { "utf-8", "ucs-bom", "euc-kr", "cp949" }
opt.encoding = "utf-8"
opt.bomb = false
opt.fileformats = { "unix", "dos" }

-- Spell
opt.spelllang = "en"
opt.spellfile = vim.fn.expand("$HOME/Dropbox/vim/spell/en.utf-8.add")

-- Project local config (.nvim.lua)
opt.exrc = true

-- Persistent undo
local tmp_dir = vim.fn.expand("$HOME/tmp")
if vim.fn.isdirectory(tmp_dir) == 0 then
  vim.fn.mkdir(tmp_dir, "p", "0770")
end

local undo_dir = tmp_dir .. "/undo_nvim"
if vim.fn.isdirectory(undo_dir) == 0 then
  vim.fn.mkdir(undo_dir, "p", "0700")
end
opt.undodir = undo_dir
opt.undofile = true

-- Search path
opt.path = { vim.fn.expand("~/develop/include"), vim.fn.expand("~/opensource/mruby/include") }

-- Ripgrep for grep
if vim.fn.executable("rg") == 1 then
  opt.grepprg = "rg --vimgrep --smart-case"
  opt.grepformat = "%f:%l:%c:%m"
end

-- CMake/C++ errorformat (fix E377 Invalid %- error)
opt.errorformat = table.concat({
  "%f:%l:%c: %t%*[^:]: %m",
  "%f:%l: %t%*[^:]: %m",
  "%f:%l:%c: %m",
  "%f:%l:%m",
  "%f(%l): %m",
}, ",")

-- FZF runtime path
opt.rtp:append("/opt/homebrew/opt/fzf")

-- Highlight settings
vim.cmd([[highlight VertSplit ctermfg=gray ctermbg=none guifg=#555555 guibg=NONE]])
