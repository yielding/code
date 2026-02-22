-- lua/plugins/tools.lua
-- Development tools and utilities

return {
  -- FZF
  {
    "junegunn/fzf",
    build = "./install --all",
  },

  {
    "junegunn/fzf.vim",
    dependencies = { "junegunn/fzf" },
    config = function()
      -- FZF keymaps
      vim.keymap.set("n", "<leader>ff", ":Files<CR>", { silent = true })
      vim.keymap.set("n", "<leader>bf", ":Buffers<CR>", { silent = true })
      vim.keymap.set("n", "<leader>gf", ":GFiles<CR>", { silent = true })
      vim.keymap.set("n", "<leader>rg", ":Rg<CR>", { silent = true })
    end,
  },

  -- TagBar: Code outline
  {
    "preservim/tagbar",
    cmd = { "TagbarToggle", "TagbarOpen", "TagbarClose" },
    config = function()
      vim.g.tagbar_width = 40
    end,
  },

  -- FuzzyFinder (legacy)
  {
    "vim-scripts/FuzzyFinder",
    dependencies = { "vim-scripts/L9" },
  },

  {
    "vim-scripts/L9",
  },

  -- vim-cmake
  {
    "cdelledonne/vim-cmake",
    ft = { "cpp", "c", "cmake" },
    config = function()
      vim.g.cmake_build_type = "Debug"
      vim.g.cmake_build_dir_location = "build"
      vim.g.cmake_root_markers = {}
      vim.g.cmake_native_build_options = { "-j10" }
      vim.g.cmake_jump_on_error = 1
      vim.g.cmake_console_size = 10
      vim.g.cmake_console_echo_cmd = 0
      vim.g.cmake_jump_on_completion = 0

      -- CMake keymaps for C++
      vim.api.nvim_create_autocmd("FileType", {
        pattern = { "cpp", "c" },
        callback = function()
          vim.keymap.set("n", "<F3>", ":CMakeGenerate debug -D CMAKE_BUILD_TYPE=Debug<CR>", { buffer = true })
          vim.keymap.set("n", "<F4>", function()
            vim.cmd("w")
            vim.cmd("cclose")
            vim.cmd("CMakeClose")
            vim.cmd("CMakeBuild")
          end, { buffer = true })
        end,
      })
    end,
  },

  -- vim-dispatch: Async build
  {
    "tpope/vim-dispatch",
  },

  -- Language specific plugins
  -- Rust
  {
    "rust-lang/rust.vim",
    ft = "rust",
    config = function()
      vim.g.rust_recommended_style = 0

      vim.api.nvim_create_autocmd("FileType", {
        pattern = "rust",
        callback = function()
          vim.keymap.set("n", "<F3>", "<Cmd>w<CR>:!cargo build<CR>", { buffer = true })
          vim.keymap.set("n", "<F4>", "<Cmd>w<CR>:!cargo run<CR>", { buffer = true })
        end,
      })
    end,
  },

  -- Ruby
  {
    "vim-ruby/vim-ruby",
    ft = "ruby",
  },

  -- TypeScript
  {
    "leafgarland/typescript-vim",
    ft = { "typescript", "typescriptreact" },
  },

  -- C++ enhanced highlight
  {
    "octol/vim-cpp-enhanced-highlight",
    ft = { "cpp", "c" },
    config = function()
      vim.g.cpp_concepts_highlight = 1
    end,
  },

  -- Kotlin
  {
    "udalov/kotlin-vim",
    ft = "kotlin",
  },

  -- cppman: C++ manual pages
  {
    "gauteh/vim-cppman",
    cmd = { "Cppman" },
  },

  -- ShowMarks
  {
    "vim-scripts/ShowMarks7",
    config = function()
      vim.g.showmarks_enable = 0
      vim.g.showmarks_include = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      vim.g.showmarks_ignore_type = "hqm"
      vim.g.showmarks_hlline_lower = 1
      vim.g.showmarks_hlline_upper = 1
    end,
  },

  -- ScrollColors: Browse colorschemes
  -- 사용법: :COLORSCROLL 또는 :SCROLLCOLOR (j/k로 이동)
  {
    "vim-scripts/ScrollColors",
    cmd = { "COLORSCROLL", "SCROLLCOLOR", "NEXTCOLOR", "PREVCOLOR", "CN", "CP" },
  },

  -- carbon-now-sh: Code screenshots
  {
    "kristijanhusak/vim-carbon-now-sh",
    cmd = { "CarbonNowSh" },
  },

  -- ack.vim
  {
    "vim-scripts/ack.vim",
    cmd = { "Ack", "AckAdd", "AckFile" },
  },

  -- quickr-cscope
  {
    "ronakg/quickr-cscope.vim",
    ft = { "c", "cpp" },
  },

  -- projectroot
  {
    "dbakker/vim-projectroot",
  },

  -- wordlist
  {
    "vim-scripts/wordlist.vim",
  },

}
