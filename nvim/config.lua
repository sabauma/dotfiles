
-- vim.notify = require('notify')

-- require('nvim-web-devicons').setup()

-- Mappings.
local on_attach = function(client, bufnr)
  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local bufopts = { noremap=true, silent=true, buffer=bufnr }
  vim.keymap.set('n', '[g', vim.diagnostic.goto_prev, bufopts)
  vim.keymap.set('n', ']g', vim.diagnostic.goto_next, bufopts)

  vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, bufopts)
  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
  vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
  vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
  vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
  vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, bufopts)
  vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
end

local configs = require('lspconfig.configs')
local lspconfig = require('lspconfig')
local lsputil = require('lspconfig.util')

lspconfig.clangd.setup {
  cmd = {"clangd", "--completion-style=detailed", "--background-index", "--background-index-priority=low"},
  on_attach = on_attach,
}

lspconfig.mlir_lsp_server.setup {
  on_attach = on_attach,
}

lspconfig.pyright.setup {
  on_attach = on_attach,
}

require('nvim-treesitter.configs').setup {
  -- Grammars are managed by nix
  -- ensure_installed = "all",
  -- ignore_install = {},
  highlight = {
    enable = true,
    disable = {},
    additional_vim_regex_highlighting = false,
  },
  indent = {
    enable = true,
  },
  incremental_selection = {
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
    },
  },
}

require('neorg').setup {
  -- Tell Neorg what modules to load
  load = {
    ["core.defaults"] = {}, -- Load all the default modules
    ["core.concealer"] = {
      config = {
        markup = {
          enabled = true
        },
        icon_preset = "basic"
      }
    }, -- Allows for use of icons
    ["core.completion"] = {
      config = {
        engine = "nvim-cmp" -- We current support nvim-compe and nvim-cmp only
      }
    },
    ["core.presenter"] = {
      config = {
        zen_mode = "zen-mode"
      },
    },
    ["core.dirman"] = {
      config = {
        workspaces = {
          work = "~/Notes/work",
          personal = "~/Notes/personal"
        }
      }
    },
    ["core.integrations.telescope"] = {}, -- Enable the telescope module
    ["core.keybinds"] = { -- Configure core.keybinds
      config = {
        default_keybinds = true, -- Generate the default keybinds
        neorg_leader = "<Leader>o" -- This is the default if unspecified
      }
    },
    ["core.qol.toc"] = { },
    ["core.export"] = { },
    ["core.export.markdown"] = {
      config = {
        extensions = "all",
      }
    },
    ["core.journal"] = {
      config = {
        journal_folder = "journal",
        strategy = "flat"
      }
    },
    ["core.esupports.metagen"] = {
      config = {
        type = "auto",
        tab = "  ",
        update_date = true
      }
    },
  },
}

-- Setup nvim-cmp.
local cmp = require('cmp')
local lspkind = require('lspkind')

cmp.setup {
  snippet = { },
  formatting = {
    format = lspkind.cmp_format(),
  },

  window = {
    completion = cmp.config.window.bordered({max_width = 65}),
    documentation = cmp.config.window.bordered({max_width = 65}),
  },

  mapping = cmp.mapping.preset.insert({
    ['<C-y>'] = cmp.config.disable, -- Specify `cmp.config.disable` if you want to remove the default `<C-y>` mapping.
    ['<C-e>'] = cmp.mapping({
      i = cmp.mapping.abort(),
      c = cmp.mapping.close(),
    }),
  }),

  sources = cmp.config.sources({
    { name = 'nvim_lsp' },
    { name = 'neorg'    },
  }, {
      { name = 'buffer' },
    })
}

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = 'path' }
  }, {
      { name = 'cmdline' }
    })
})

local previewers = require('telescope.previewers')
require('telescope').setup {
  defaults = {
    layout_strategy = 'vertical',
    layout_config = { vertical = { width = 0.7 } },
  },
}

require('telescope').load_extension('media_files')

require("zen-mode").setup {
  window = {
    width = 90,
    options = {
      number = false
    }
  }
}

require("lualine").setup()

