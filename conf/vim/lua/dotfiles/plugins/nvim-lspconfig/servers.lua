local runtime_path = vim.split(package.path, ";")
table.insert(runtime_path, "lua/?.lua")
table.insert(runtime_path, "lua/?/init.lua")

local server_map = {
	{
		servers = {
			["null-ls"] = {
				provides = {
					"diagnostics",
					"formatting",
				},
			},
		},
		pattern = {
			"*.bash",
			"*.css",
			"*.fish",
			"*.html,",
			"*.js",
			"*.json",
			"*.jsonc",
			"*.jsx",
			"*.less",
			"*.lua,",
			"*.md",
			"*.org",
			"*.py",
			"*.rb",
			"*.scss",
			"*.sh",
			"*.tex",
			"*.ts",
			"*.tsx",
			"*.vim",
			"*.vue",
			"*.yml,",
		},
	},
	{
		pattern = { "*.jsx", "*.js", "*.ts", "*.tsx" },
		servers = {
			flow = {
				provides = {
					"snippets",
					"diagnostics",
					"formatting",
				},
				autostart = false,
			},
			tsserver = {
				flags = {
					debounce_text_changes = 500,
				},
			},
		},
	},
	{
		pattern = "*.lua",
		servers = {
			sumneko_lua = {
				provides = {
					"snippets",
				},
				settings = {
					Lua = {
						runtime = {
							version = "LuaJIT",
							path = runtime_path,
						},
						workspace = {
							library = vim.api.nvim_get_runtime_file("", true),
						},
						completion = {
							callSnippet = "Replace",
							keywordSnippet = "Replace",
						},
						diagnostics = {
							globals = { "vim" },
						},
						telemetry = {
							enable = false,
						},
					},
				},
				flags = {
					debounce_text_changes = 500,
				},
			},
		},
	},
	{
		pattern = { "*.css", "*.scss" },
		servers = {
			cssls = {
				provides = {
					"snippets",
					"diagnostics",
				},
				flags = {
					debounce_text_changes = 500,
				},
			},
		},
	},
	{
		pattern = "*.html",
		servers = {
			html = {
				provides = {
					"snippets",
				},
				flags = {
					debounce_text_changes = 500,
				},
			},
		},
	},
	{
		pattern = { "*.jsonc", "*.json" },
		servers = {
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
		},
	},
	{ pattern = "*.rb", servers = { solargraph = {} } },
	{
		pattern = "*.vim",
		servers = {
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
		},
	},
	{
		pattern = "*.bash, *.sh, *.zsh",
		servers = {
			bashls = {
				flags = {
					debounce_text_changes = 500,
				},
			},
		},
	},
	{
		pattern = "*.py",
		servers = {
			pyright = {
				flags = {
					debounce_text_changes = 500,
				},
			},
		},
	},
}

local cache = {}
return setmetatable(server_map, {
	__index = function(_, idx)
		if cache[idx] ~= nil then
			return cache[idx]
		end
		for _, server_block in pairs(server_map) do
			if vim.fn.has_key(server_block.servers, idx) == 1 then
				cache[idx] = server_block.servers[idx]
				return server_block.servers[idx]
			end
		end
		return nil
	end,
})
