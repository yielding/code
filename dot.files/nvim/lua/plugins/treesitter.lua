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

      -- 자동 설치할 파서 목록
      local parsers = {
        'vim', 'vimdoc', 'lua', 'query', 'regex', 'bash',
        'markdown', 'markdown_inline', 'python', 'cpp', 'c',
        'rust', 'ruby', 'javascript', 'typescript', 'tsx',
        'json', 'yaml', 'html', 'css', 'go', 'kotlin',
      }
      require('nvim-treesitter').install(parsers)

      -- Register crystal filetype
      vim.filetype.add({
        extension = {
          cr = "crystal",
        },
      })

      -- Crystal: tree-sitter based syntax highlighting
      vim.api.nvim_create_autocmd("FileType", {
        pattern = { "crystal" },
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
