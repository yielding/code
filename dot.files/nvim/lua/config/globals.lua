-- lua/config/globals.lua
-- Global variables and provider settings

-- Shortcut paths for quick editing
vim.g._vimrc = "~/.config/nvim/init.lua"
vim.g._tmuxrc = "~/.tmux.conf"
vim.g._zshrc = "~/.zshrc"
vim.g._plugs = "~/.config/nvim/lua/plugins/init.lua"
vim.g._test = "~/snippets/"
vim.g._config = "~/.config/nvim/lua/config/"
vim.g._plugin = "~/.config/nvim/lua/plugins/"
vim.g._mysnippet = "~/.config/nvim/luasnippets/"
vim.g._snippet = "~/.config/nvim/autoload/plugged/vim-snippets/snippets/"

-- Provider settings
vim.g.python3_host_prog = "/usr/bin/python3"
vim.g.ruby_host_prog = "/home/yielding/.rubies/ruby-4.0.0/bin/neovim-ruby-host"
vim.g.loaded_perl_provider = 0

-- Disable netrw (using nvim-tree instead)
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- C++ syntax highlighting
vim.g.cpp_concepts_highlight = 1

-- Rust
vim.g.rust_recommended_style = 0

-- Bracket matching
vim.g.loaded_matchparen = 0

-- CMake settings
vim.g.cmake_build_type = "Debug"
vim.g.cmake_build_dir_location = "build"
vim.g.cmake_root_markers = {}
vim.g.cmake_native_build_options = { "-j10" }

-- Check nvim vs vim
vim.g.is_nvim = vim.fn.has("nvim") == 1
vim.g.is_vim8 = vim.version().minor >= 8 and 1 or 0
