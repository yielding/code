-- lua/config/autocmds.lua
-- Autocommands

local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

-- General
augroup("GeneralSettings", { clear = true })

-- Quickfix auto open after grep
autocmd("QuickFixCmdPost", {
  group = "GeneralSettings",
  pattern = "grep",
  command = "copen",
})

-- Syntax sync from start
autocmd("BufEnter", {
  group = "GeneralSettings",
  pattern = "*",
  command = "syntax sync fromstart",
})

-- Auto check for file changes when focus returns
autocmd({ "FocusGained", "BufEnter" }, {
  group = "GeneralSettings",
  pattern = "*",
  command = "checktime",
})

-- JSON comment syntax
autocmd("FileType", {
  group = "GeneralSettings",
  pattern = "json",
  command = [[syntax match Comment +\/\/.\+$+]],
})

-- Auto switch to English input on InsertLeave (macOS)
autocmd("InsertLeave", {
  group = "GeneralSettings",
  callback = function()
    vim.fn.system("im-select com.apple.keylayout.ABC")
  end,
})

-- File type specific settings
augroup("FileTypeSettings", { clear = true })

autocmd({ "BufNewFile", "BufReadPost" }, {
  group = "FileTypeSettings",
  pattern = "*.rb",
  callback = function()
    vim.opt_local.foldmethod = "expr"
  end,
})

autocmd({ "BufNewFile", "BufReadPost" }, {
  group = "FileTypeSettings",
  pattern = "*.py",
  callback = function()
    vim.opt_local.tabstop = 2
    vim.opt_local.softtabstop = 2
    vim.opt_local.shiftwidth = 2
    vim.opt_local.textwidth = 0
    vim.cmd("compiler pyunit")
  end,
})

autocmd({ "BufNewFile", "BufReadPost" }, {
  group = "FileTypeSettings",
  pattern = "*.g4",
  callback = function()
    vim.opt_local.filetype = "antlr"
  end,
})

-- Ruby configuration
augroup("RubySettings", { clear = true })

autocmd("FileType", {
  group = "RubySettings",
  pattern = "ruby",
  callback = function()
    vim.cmd("filetype plugin indent on")
    -- Ruby eval function
    vim.keymap.set("n", "<F5>", function()
      local src = vim.fn.tempname()
      vim.cmd("silent write " .. src)
      vim.cmd("below pedit! Ruby Output")
      vim.cmd("wincmd P")
      vim.bo.buftype = "nofile"
      vim.bo.swapfile = false
      vim.bo.syntax = ""
      vim.bo.bufhidden = "delete"
      vim.cmd("silent %!ruby --jit " .. src .. " 2>&1")
      vim.cmd("wincmd p")
    end, { buffer = true })
    vim.keymap.set("n", "<leader>tag", ":!ripper-tags -R.<CR>", { buffer = true })
  end,
})

-- Language-specific run commands
augroup("LanguageRun", { clear = true })

autocmd("FileType", {
  group = "LanguageRun",
  pattern = "python",
  callback = function()
    vim.keymap.set("n", "<C-S-r>", ":!python3 %<CR>", { buffer = true })
  end,
})

autocmd("FileType", {
  group = "LanguageRun",
  pattern = "cs",
  callback = function()
    vim.keymap.set("n", "<C-S-b>", ":!dotnet build<CR>", { buffer = true })
    vim.keymap.set("n", "<C-S-r>", ":!dotnet run<CR>", { buffer = true })
  end,
})

autocmd("FileType", {
  group = "LanguageRun",
  pattern = "java",
  callback = function()
    vim.keymap.set("n", "<C-S-b>", ":!javac %<CR>", { buffer = true })
    vim.keymap.set("n", "<C-S-r>", function()
      vim.cmd("!java " .. vim.fn.expand("%:r"))
    end, { buffer = true })
  end,
})

autocmd("FileType", {
  group = "LanguageRun",
  pattern = "haskell",
  callback = function()
    vim.keymap.set("n", "<C-S-r>", ":!runhaskell %<CR>", { buffer = true })
  end,
})

autocmd("FileType", {
  group = "LanguageRun",
  pattern = "javascript",
  callback = function()
    vim.keymap.set("n", "<C-S-r>", ":!node %<CR>", { buffer = true })
  end,
})

autocmd("FileType", {
  group = "LanguageRun",
  pattern = "kotlin",
  callback = function()
    vim.keymap.set("n", "<C-S-r>", function()
      vim.cmd("!kot.sh " .. vim.fn.expand("%"))
    end, { buffer = true })
  end,
})

-- CMake autocommands
augroup("CMakeSettings", { clear = true })

autocmd("User", {
  group = "CMakeSettings",
  pattern = "CMakeBuildSucceeded",
  callback = function()
    -- Close quickfix if open
    local qf_open = false
    for _, win in ipairs(vim.fn.getwininfo()) do
      if win.quickfix == 1 then
        qf_open = true
        break
      end
    end
    if qf_open then
      vim.cmd("cclose")
    end
    vim.cmd("CMakeClose")
  end,
})

autocmd("User", {
  group = "CMakeSettings",
  pattern = "CMakeBuildFailed",
  callback = function()
    vim.cmd("rightbelow vsplit")
    vim.cmd("wincmd l")
    vim.cmd("copen")
  end,
})
