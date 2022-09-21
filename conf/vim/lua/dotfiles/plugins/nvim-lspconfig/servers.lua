-- put this file somewhere in your nvim config, like: ~/.config/nvim/lua/config/lua-lsp.lua
-- usage: require'lspconfig'.sumneko_lua.setup(require("config.lua-lsp"))

local library = {}

local runtime_path = vim.split(package.path, ";")

-- this is the ONLY correct way to setup your path
table.insert(runtime_path, "lua/?.lua")
table.insert(runtime_path, "lua/?/init.lua")

local function add(lib)
	for _, p in pairs(vim.fn.expand(lib, false, true)) do
		p = vim.loop.fs_realpath(p)
		if p ~=nil then
			library[p] = true
		end
	end
end

-- add runtime
add("$VIMRUNTIME")

-- add your config
add("~/dotfiles/conf/vim")

-- add plugins
-- if you're not using packer, then you might need to change the paths below
add("~/.local/share/packer.nvim/pack/packer/opt/*")
add("~/.local/share/packer.nvim/pack/packer/start/*")

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
				on_new_config = function(config, root)
					local libs = vim.tbl_deep_extend("force", {}, library)
					libs[root] = nil
					config.settings.Lua.workspace.library = libs
					return config
				end,
				settings = {
					Lua = {
						runtime = {
							-- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
							version = "LuaJIT",
							-- Setup your lua path
							path = runtime_path,
						},
						completion = { callSnippet = "Both" },
						diagnostics = {
							-- Get the language server to recognize the `vim` global
							globals = { "vim" },
						},
						workspace = {
							-- Make the server aware of Neovim runtime files
							library = library,
							maxPreload = 2000,
							preloadFileSize = 50000,
						},
						-- Do not send telemetry data containing a randomized but unique identifier
						telemetry = { enable = false },
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
		if idx == "servers" then
			local output = {}
			for _, server_block in pairs(server_map) do
				for name, server in pairs(server_block.servers) do
					cache[name] = server
					table.insert(output, name)
				end
			end
			return output
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
