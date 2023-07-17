return {
	bashls = {
		flags = {
			debounce_text_changes = 500,
		},
		pattern = { "*.bash", "*.sh", "*.zsh" },
	},
	cssls = {
		flags = {
			debounce_text_changes = 500,
		},
		pattern = { "*.css", "*.scss" },
		snippets = true,
	},
	html = {
		flags = {
			debounce_text_changes = 500,
		},
		pattern = "*.html",
		snippets = true,
	},
	jsonls = {
		filetypes = { "json", "jsonc" },
		flags = {
			debounce_text_changes = 500,
		},
		pattern = { "*.jsonc", "*.json" },
		snippets = true,
	},
	lua_ls = {
		pattern = "*.lua",
		snippets = true,
		settings = {
			Lua = {
				completion = { callSnippet = "Both" },
				workspace = {
					-- Make the server aware of Neovim runtime files
					checkThirdParty = false,
				},
				-- Do not send telemetry data containing a randomized but unique identifier
				telemetry = { enable = false },
			},
		},
		flags = {
			debounce_text_changes = 500,
		},
	},
	pyright = {
		flags = {
			debounce_text_changes = 500,
		},
		pattern = "*.py",
	},
	standardrb = {
		pattern = "*.rb",
		single_file_support = true,
	},
	tsserver = {
		flags = {
			debounce_text_changes = 500,
		},
		pattern = { "*.jsx", "*.js", "*.ts", "*.tsx" },
	},
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
		pattern = "*.vim",
		snippets = true,
	},
	yamlls = {
		pattern = { "*.yaml", "*.yml" },
	}
}
