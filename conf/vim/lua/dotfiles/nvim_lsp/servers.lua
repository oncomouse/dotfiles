-- luacheck: globals vim
local servers = {
	["null-ls"] = {
		provides = {
			"diagnostics",
			"formatting",
		},
	},
	flow = {
		provides = {
			"snippets",
			"diagnostics",
			"formatting",
		},
		autostart = require("lspconfig.util").path.exists(".flowconfig"),
	},
	sumneko_lua = {
		provides = {
			"snippets",
		},
		settings = {
			Lua = {
				runtime = {
					version = "LuaJIT",
					path = vim.split(package.path, ";"),
				},
				workspace = {
					library = {
						[vim.fn.expand("$VIMRUNTIME/lua")] = true,
						[vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
					},
				},
			},
		},
		flags = {
			debounce_text_changes = 500,
		},
	},
	cssls = {
		provides = {
			"snippets",
			"diagnostics",
		},
		flags = {
			debounce_text_changes = 500,
		},
	},
	html = {
		provides = {
			"snippets",
		},
		flags = {
			debounce_text_changes = 500,
		},
	},
	jsonls = {
		filetypes = { "json", "jsonc" },
		provides = {
			"snippets",
			"diagnostics",
		},
		flags = {
			debounce_text_changes = 500,
		},
	},
	solargraph = {},
	vimls = {
		flags = {
			debounce_text_changes = 500,
		},
		init_options = {
			isNeovim = true,
			diagnostic = {
				enable = false,
			},
		},
		provides = {
			"snippets",
		},
	},
	bashls = {
		flags = {
			debounce_text_changes = 500,
		},
	},
	pyright = {
		flags = {
			debounce_text_changes = 500,
		},
	},
	tsserver = {
		flags = {
			debounce_text_changes = 500,
		},
	},
}
return servers
