-- lua/plugins/treesitter.lua
-- Treesitter configuration
---@diagnostic disable: missing-fields

return {
  {
    "nvim-treesitter/nvim-treesitter",
    branch = "main",
    build = ":TSUpdate",
    config = function()
      require("nvim-treesitter").setup({})

      -- Fix: remove invalid "substitute" node from vim highlights query at runtime
      local ok, query_src = pcall(function()
        local files = vim.api.nvim_get_runtime_file("queries/vim/highlights.scm", true)
        local parts = {}
        for _, f in ipairs(files) do
          local fh = io.open(f, "r")
          if fh then
            local content = fh:read("*a")
            fh:close()
            table.insert(parts, content)
          end
        end
        return table.concat(parts, "\n")
      end)
      if ok and query_src and query_src:find('"substitute"') then
        local patched = query_src:gsub('%s*"substitute"\n', "\n")
        vim.treesitter.query.set("vim", "highlights", patched)
      end

      -- 자동 설치할 파서 목록
      local parsers = {
        'vim', 'vimdoc', 'lua', 'query', 'regex', 'bash',
        'markdown', 'markdown_inline', 'python', 'cpp', 'c',
        'rust', 'ruby', 'javascript', 'typescript', 'tsx',
        'json', 'yaml', 'html', 'css', 'go', 'kotlin', 'sql', 'c_sharp',
        'cmake', 'dockerfile', 'toml', 'haskell', 'pascal',
      }
      require('nvim-treesitter').install(parsers)

      -- Register custom filetypes
      -- Note: haxe parser manually installed (vantreeseba/tree-sitter-haxe)
      vim.filetype.add({
        extension = {
          cr = "crystal",
          hx = "haxe",
        },
      })

      -- Tree-sitter based syntax highlighting for all supported languages
      vim.api.nvim_create_autocmd("FileType", {
        pattern = {
          "vim", "lua", "bash", "sh", "zsh",
          "markdown", "python", "cpp", "c",
          "rust", "ruby", "javascript", "typescript", "typescriptreact",
          "json", "yaml", "html", "css", "go", "kotlin", "sql", "cs",
          "cmake", "dockerfile", "toml", "haskell", "crystal", "java", "pascal", "haxe",
        },
        callback = function()
          pcall(vim.treesitter.start)
        end,
      })

      -- Treesitter based folding for supported languages
      vim.api.nvim_create_autocmd("FileType", {
        callback = function()
          local ok, lang = pcall(vim.treesitter.language.get_lang, vim.bo.filetype)
          if ok and lang then
            local has_parser = pcall(vim.treesitter.language.add, lang)
            if has_parser then
              vim.wo.foldmethod = "expr"
              vim.wo.foldexpr = "v:lua.vim.treesitter.foldexpr()"
              vim.wo.foldlevel = 99
            end
          end
        end,
      })
    end,
  },
}
