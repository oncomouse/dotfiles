local M = {}

local library = {}

local runtime_path = vim.split(package.path, ";")

table.insert(runtime_path, "lua/?.lua")
table.insert(runtime_path, "lua/?/init.lua")

local function add(lib)
	for _, p in pairs(vim.fn.expand(lib, false, true)) do
		p = vim.loop.fs_realpath(p)
		if p ~= nil then
			library[p] = true
		end
	end
end

-- Neovim Runtime:
add("$VIMRUNTIME")

-- Dotfiles:
add("~/dotfiles/conf/vim")

-- Turns off complaining about luassert:
library["${3rd}/luassert/library"] = true
library["${3rd}/busted/library"] = true

-- Load plugins (this causes huge CPU usage on macOS):
if vim.fn.has("mac") ~= 1 and os.getenv("DOTFILES_TARGET") ~= "laptop" then
	add("~/.local/share/packer.nvim/pack/packer/opt/*")
	add("~/.local/share/packer.nvim/pack/packer/start/*")
end

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
			"*.html",
			"*.js",
			"*.json",
			"*.jsonc",
			"*.jsx",
			"*.less",
			"*.lua",
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
			"*.yml",
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
		pattern = { "*.bash", "*.sh", "*.zsh" },
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

M = setmetatable(server_map, {
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
			cache.servers = output
			return output
		end
		if idx == "pattern" then
			local output = {}
			for _, server_block in pairs(server_map) do
				for _, pattern in
					pairs(type(server_block.pattern) == "string" and { server_block.pattern } or server_block.pattern)
				do
					table.insert(output, pattern)
				end
			end
			cache.pattern = table.concat(vim.fn.uniq(vim.fn.sort(output)), ",")
			return cache.pattern
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

return M

