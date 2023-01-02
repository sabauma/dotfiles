
vim.notify = require('notify')

require('nvim-treesitter.configs').setup {
  --ensure_installed = { "norg", "norg_meta", "norg_table", "haskell", "cpp", "c", "javascript", "vim", "lua" },
  ensure_installed = "all",
  ignore_install = {}, -- List of parsers to ignore installing
  highlight = {
    enable = true,              -- false will disable the whole extension
    disable = {},  -- list of language that will be disabled
    -- Setting this to true will run `:h syntax` and tree-sitter at the same time.
    -- Set this to `true` if you depend on 'syntax' being enabled (like for indentation).
    -- Using this option may slow down your editor, and you may see some duplicate highlights.
    -- Instead of true it can also be a list of languages
    additional_vim_regex_highlighting = true,
  },
  indent = {
    enable = true,
  },
  incremental_selection = {
    enable = true,
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
    ["core.norg.concealer"] = {
      config = {
        markup = {
          enabled = true
        },
        icon_preset = "diamond"
      }
    }, -- Allows for use of icons
    ["core.norg.completion"] = {
      config = {
        engine = "nvim-cmp" -- We current support nvim-compe and nvim-cmp only
      }
    },
    ["core.presenter"] = {
      config = {
        zen_mode = "zen-mode"
      },
    },
    ["core.norg.dirman"] = {
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
    ["core.norg.qol.toc"] = { },
    ["core.export"] = { },
    ["core.export.markdown"] = {
      config = {
        extensions = "all",
      }
    },
    ["core.norg.journal"] = {
      config = {
        journal_folder = "journal",
        strategy = "flat"
      }
    },
    ["core.norg.esupports.metagen"] = {
      config = {
        update_date = true
      }
    },
    ["core.norg.news"] = {
      config = {
        check_news = false,
      },
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

require('telescope').setup {
  defaults = {
    layout_strategy = 'vertical',
    layout_config = { vertical = { width = 0.7 } },
  },
}

require('telescope').load_extension('media_files')
require('spellsitter').setup {
  enable = true,
}

require("zen-mode").setup {
  window = {
    width = 90,
    options = {
      number = false
    }
  }
}

require("lualine").setup()

-- Enable telescope theme
vim.g.gruvbox_baby_telescope_theme = 1
vim.g.gruvbox_baby_keyword_style = "italic"
vim.g.gruvbox_baby_background_color = "dark"
vim.g.gruvbox_baby_user_original_palette = true
