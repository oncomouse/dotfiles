local M = {}

local function on_attach(client, buf_num)
	-- Update codeLens:
	if client.server_capabilities.codeLensProvider then
		vim.api.nvim_create_autocmd("CursorHold,CursorHoldI,InsertLeave", {
			buffer = buf_num,
			group = "dotfiles-settings",
			callback = function()
				vim.lsp.codelens.refresh()
			end,
		})
	end
	-- Use C+x C+o for completion:
	if client.server_capabilities.completionProvider then
		vim.opt_local.omnifunc = "v:lua.vim.lsp.omnifunc"
	end
	-- Use C+x C+] for tags:
	if client.server_capabilities.definitionProvider then
		vim.opt_local.tagfunc = "v:lua.vim.lsp.tagfunc"
	end
	-- Use K for hover (except in VimL):
	if vim.opt_local.ft:get() ~= "vim" then
		vim.api.nvim_buf_create_user_command(buf_num, "Hover", function()
			vim.lsp.buf.hover()
		end, {
			desc = "lua vim.lsp.buf.hover()",
			force = true,
			nargs = "+",
		})
		vim.opt_local.keywordprg = ":Hover"
	end
	-- LSP-specific maps:
	vim.keymap.set("n", "<leader>s", vim.lsp.buf.document_symbol, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.document_symbol()",
	})
	vim.keymap.set("n", "<F2>", vim.lsp.buf.rename, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.rename()",
	})
	vim.keymap.set("n", "<leader>gd", vim.lsp.buf.definition, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.definition()",
	})
	vim.keymap.set("n", "<leader>gD", vim.lsp.buf.declaration, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.declaration()",
	})
	vim.keymap.set("n", "<leader>gy", vim.lsp.buf.type_definition, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.type_definition()",
	})
	vim.keymap.set("n", "<leader>gi", vim.lsp.buf.implementation, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.implementation()",
	})
	vim.keymap.set("n", "<leader>gr", vim.lsp.buf.references, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.references()",
	})
	vim.keymap.set("n", "<leader>gl", vim.lsp.codelens.run, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.codelens.run()",
	})
	vim.keymap.set("n", "<leader>ga", vim.lsp.buf.code_action, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.code_action()",
	})
	vim.keymap.set("v", "<leader>ga", function()
		local _, s_row, s_col, _ = unpack(vim.fn.getpos("v"))
		local _, e_row, e_col, _ = unpack(vim.fn.getcurpos())
		if vim.fn.mode() == "V" then
			s_col = 1
			e_col = #(vim.fn.getline(e_row))
		end
		vim.lsp.buf.code_action({
			range = {
				start = { s_row, s_col },
				["end"] = { e_row, e_col },
			},
		})
	end, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "'<,'>lua vim.lsp.buf.code_action()",
	})
	vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.signature_help()",
	})
	local diagnostic_provider = vim.tbl_contains(M[client.name].provides or {}, "diagnostics")
	local formatting_provider = vim.tbl_contains(M[client.name].provides or {}, "formatting")
	if diagnostic_provider then
		vim.api.nvim_create_autocmd("DiagnosticChanged,BufEnter", {
			buffer = buf_num,
			group = "dotfiles-settings",
			callback = function()
				vim.diagnostic.setloclist({ open = false })
			end,
		})
		vim.keymap.set("n", "]d", vim.diagnostic.goto_next, {
			silent = true,
			noremap = true,
			buffer = true,
			desc = "lua vim.diagnostic.goto_next()",
		})
		vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, {
			silent = true,
			noremap = true,
			buffer = true,
			desc = "lua vim.diagnostic.goto_prev()",
		})
	end
	-- Formatting:
	if formatting_provider then
		vim.api.nvim_buf_create_user_command(buf_num, "Format", function()
			vim.lsp.buf.format({
				async = true,
				filter = function(c)
					return vim.tbl_contains(M[c.name].provides or {}, "formatting")
				end,
			})
		end, {
			desc = "lua vim.lsp.buf.format()",
			force = true,
		})
	else
		client.server_capabilities.documentFormattingProvider = false
		client.server_capabilities.documentRangeFormattingProvider = false
	end
end

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

local cache = {
	on_attach = on_attach,
}

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
