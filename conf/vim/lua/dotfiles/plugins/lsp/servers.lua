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
	add("~/.local/share/nvim/lazy/*")
	add("~/.local/share/nvim/lazy/*")
end

return {
	tsserver = {
		flags = {
			debounce_text_changes = 500,
		},
		pattern = { "*.jsx", "*.js", "*.ts", "*.tsx" },
	},
	lua_ls = {
		pattern = "*.lua",
		snippets = true,
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
	solargraph = {
		pattern = "*.rb",
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
	bashls = {
		flags = {
			debounce_text_changes = 500,
		},
		pattern = { "*.bash", "*.sh", "*.zsh" },
	},
	pyright = {
		flags = {
			debounce_text_changes = 500,
		},
		pattern = "*.py",
	},
}
