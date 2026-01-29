-- lua/plugins/lsp.lua
-- LSP configuration with mason (Neovim 0.11+ compatible)

return {
  -- Mason: LSP server installer
  {
    "williamboman/mason.nvim",
    build = ":MasonUpdate",
    config = function()
      require("mason").setup({
        ui = {
          icons = {
            package_installed = "✓",
            package_pending = "➜",
            package_uninstalled = "✗",
          },
        },
      })
    end,
  },

  -- Mason-lspconfig: Bridge between mason and lspconfig
  {
    "williamboman/mason-lspconfig.nvim",
    dependencies = { "williamboman/mason.nvim" },
    config = function()
      require("mason-lspconfig").setup({
        ensure_installed = {
          "clangd",           -- C/C++
          "rust_analyzer",    -- Rust
          "pyright",          -- Python
          "ruby_lsp",         -- Ruby
          "ts_ls",            -- TypeScript/JavaScript
          "jsonls",           -- JSON
          "html",             -- HTML
          "cssls",            -- CSS
          "cmake",            -- CMake
          "lua_ls",           -- Lua
          "kotlin_language_server", -- Kotlin
          "hls",              -- Haskell
          "crystalline",      -- Crystal
          "jdtls",            -- Java
          "gopls",            -- Go
        },
        automatic_installation = true,
      })
    end,
  },

  -- nvim-lspconfig: LSP client configuration (Neovim 0.11+ using vim.lsp.config)
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "hrsh7th/cmp-nvim-lsp",
    },
    config = function()
      -- Diagnostic configuration
      vim.diagnostic.config({
        virtual_text = true,
        signs = true,
        underline = true,
        update_in_insert = false,
        severity_sort = true,
      })

      -- Get capabilities from nvim-cmp
      local capabilities = require("cmp_nvim_lsp").default_capabilities()

      -- LSP keymaps setup function
      local function setup_lsp_keymaps(bufnr)
        local opts = { buffer = bufnr, noremap = true, silent = true }
        local map = vim.keymap.set

        -- Navigation
        map("n", "gd", vim.lsp.buf.definition, opts)
        map("n", "gD", vim.lsp.buf.declaration, opts)
        map("n", "gy", vim.lsp.buf.type_definition, opts)
        map("n", "gi", vim.lsp.buf.implementation, opts)
        map("n", "gr", vim.lsp.buf.references, opts)
        map("n", "K", vim.lsp.buf.hover, opts)
        map("n", "<C-k>", vim.lsp.buf.signature_help, opts)

        -- Actions
        map("n", "rn", vim.lsp.buf.rename, opts)
        map({ "n", "v" }, "<leader>ca", vim.lsp.buf.code_action, opts)
        map("n", "<leader>dd", vim.diagnostic.open_float, opts)

        -- Diagnostics navigation (Neovim 0.11+)
        map("n", "[d", function() vim.diagnostic.jump({ count = -1 }) end, opts)
        map("n", "]d", function() vim.diagnostic.jump({ count = 1 }) end, opts)

        -- Formatting
        map("n", "<leader>fm", function()
          vim.lsp.buf.format({ async = true })
        end, opts)

        -- clang-tidy fix
        map("n", "<leader>ft", function()
          local file = vim.fn.expand("%:p")
          vim.cmd("write")
          vim.fn.system("clang-tidy --fix-errors --extra-arg=-std=c++26 " .. file)
          vim.cmd("edit!")
        end, opts)
      end

      -- LspAttach autocommand for keymaps and highlighting
      vim.api.nvim_create_autocmd("LspAttach", {
        group = vim.api.nvim_create_augroup("UserLspConfig", { clear = true }),
        callback = function(ev)
          local bufnr = ev.buf
          local client = vim.lsp.get_client_by_id(ev.data.client_id)

          -- Setup keymaps
          setup_lsp_keymaps(bufnr)

          -- Highlight references on cursor hold
          if client and client.server_capabilities.documentHighlightProvider then
            vim.api.nvim_create_autocmd("CursorHold", {
              buffer = bufnr,
              callback = function()
                vim.lsp.buf.document_highlight()
              end,
            })
            vim.api.nvim_create_autocmd("CursorMoved", {
              buffer = bufnr,
              callback = function()
                vim.lsp.buf.clear_references()
              end,
            })
          end
        end,
      })

      -- Server configurations using vim.lsp.config (Neovim 0.11+)
      local servers = {
        clangd = {
          cmd = {
            "clangd",
            "--background-index",
            "--clang-tidy",
            "--header-insertion=iwyu",
            "--completion-style=detailed",
            "--function-arg-placeholders=true",
            "--compile-commands-dir=build/debug",
          },
        },
        rust_analyzer = {
          settings = {
            ["rust-analyzer"] = {
              checkOnSave = true,
              check = {
                command = "clippy",
              },
              cargo = {
                allFeatures = true,
              },
            },
          },
        },
        pyright = {
          settings = {
            python = {
              analysis = {
                autoSearchPaths = true,
                diagnosticMode = "workspace",
                useLibraryCodeForTypes = true,
              },
            },
          },
        },
        ruby_lsp = {},
        ts_ls = {},
        jsonls = {},
        html = {},
        cssls = {},
        cmake = {},
        lua_ls = {
          settings = {
            Lua = {
              runtime = { version = "LuaJIT" },
              diagnostics = { globals = { "vim" } },
              workspace = {
                library = vim.api.nvim_get_runtime_file("", true),
                checkThirdParty = false,
              },
              telemetry = { enable = false },
            },
          },
        },
        kotlin_language_server = {},
        hls = {},
        crystalline = {},
        gopls = {},
        -- jdtls는 nvim-jdtls가 관리
      }

      -- Setup each server using vim.lsp.config (Neovim 0.11+)
      for server, config in pairs(servers) do
        config.capabilities = capabilities
        vim.lsp.config[server] = config
      end

      -- Enable all configured servers
      vim.lsp.enable({
        "clangd",
        "rust_analyzer",
        "pyright",
        "ruby_lsp",
        "ts_ls",
        "jsonls",
        "html",
        "cssls",
        "cmake",
        "lua_ls",
        "kotlin_language_server",
        "hls",
        "crystalline",
        "gopls",
        -- jdtls는 nvim-jdtls가 관리
      })
    end,
  },

  -- nvim-jdtls: Enhanced Java support
  {
    "mfussenegger/nvim-jdtls",
    ft = "java",
    dependencies = {
      "williamboman/mason.nvim",
      "hrsh7th/cmp-nvim-lsp",
    },
    config = function()
      local jdtls_path = vim.fn.stdpath("data") .. "/mason/packages/jdtls"
      local launcher_jar = vim.fn.glob(jdtls_path .. "/plugins/org.eclipse.equinox.launcher_*.jar")

      if launcher_jar == "" then
        vim.notify("jdtls not installed. Run :MasonInstall jdtls", vim.log.levels.WARN)
        return
      end

      local project_name = vim.fn.fnamemodify(vim.fn.getcwd(), ":p:h:t")
      local workspace_dir = vim.fn.stdpath("data") .. "/jdtls-workspace/" .. project_name

      local config = {
        cmd = {
          "java",
          "-Declipse.application=org.eclipse.jdt.ls.core.id1",
          "-Dosgi.bundles.defaultStartLevel=4",
          "-Declipse.product=org.eclipse.jdt.ls.core.product",
          "-Xmx1g",
          "--add-modules=ALL-SYSTEM",
          "--add-opens", "java.base/java.util=ALL-UNNAMED",
          "--add-opens", "java.base/java.lang=ALL-UNNAMED",
          "-jar", launcher_jar,
          "-configuration", jdtls_path .. "/config_linux",
          "-data", workspace_dir,
        },
        root_dir = require("jdtls.setup").find_root({ ".git", "mvnw", "gradlew", "pom.xml", "build.gradle" })
            or vim.fn.getcwd(),
        settings = {
          java = {
            signatureHelp = { enabled = true },
            completion = {
              favoriteStaticMembers = {
                "org.junit.Assert.*",
                "org.junit.jupiter.api.Assertions.*",
              },
            },
          },
        },
        capabilities = require("cmp_nvim_lsp").default_capabilities(),
      }

      ---@diagnostic disable-next-line: missing-fields
      require("jdtls").start_or_attach(config)
    end,
  },

}
