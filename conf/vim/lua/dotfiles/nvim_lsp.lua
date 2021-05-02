-- luacheck: globals vim
local lspconfig = require('lspconfig')
-- vim.lsp.set_log_level('debug')

-- Disable diagnostics
local no_diagnostics = {
	["textDocument/publishDiagnostics"] = function() end
}

local on_attach = function(_, bufnr)
	-- Once codelens is setup:
	-- vim.api.nvim_command [[autocmd CursorHold,CursorHoldI,InsertLeave <buffer> lua vim.lsp.codelens.refresh()]]
	vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
	vim.api.nvim_buf_set_option(bufnr, "formatexpr", "v:lua.lsp_formatexpr")
	vim.api.nvim_command("call dotfiles#autocomplete#nvim_lsp#attach()")
end

-- 'tsserver',
local servers = {
	'vimls',
	'bashls',
	'solargraph',
}
for _, lsp in ipairs(servers) do
	lspconfig[lsp].setup {
		on_attach = on_attach,
		handlers = no_diagnostics,
	}
end

lspconfig.sumneko_lua.setup{
	on_attach = on_attach,
	cmd = {'sumneko-lua-language-server'},
	handlers = no_diagnostics,
	settings = {
		Lua = {
			-- diagnostics = {
			-- 	enable = true,
			-- 	globals = {'vim'},
			-- 	disable = { 'lowercase-global' },
			-- },
			runtime = {
				version = "LuaJIT",
				path = vim.split(package.path, ";")
			},
			workspace = {
				library = {
					[vim.fn.expand("$VIMRUNTIME/lua")] = true,
					[vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true
				}
			},
		}
	}
}

lspconfig.citation_lsp.setup{
	on_attach = on_attach,
	handlers = no_diagnostics,
	settings = {
		citation = {
			bibliographies = {
			'~/Seadrive/My Libraries/My Library/Documents/Academic Stuff/library.bib'
			}
		}
	}
}
lspconfig.efm.setup {
	root_dir = lspconfig.util.root_pattern("Rakefile", "yarn.lock", "lerna.json", ".git", "poetry.toml"),
	filetypes = {
		'css',
		'html',
		'javascript',
		'json',
		'lua',
		'markdown',
		'python',
		'ruby',
		'sh',
		'vim',
	},
	handlers = {
		["textDocument/publishDiagnostics"] = vim.lsp.with(
			vim.lsp.diagnostic.on_publish_diagnostics, {
			signs = false,
			underline = true,
			update_in_insert = false,
			virtual_text = false,
			}
		)
	},
	init_options = {
		documentFormatting = true,
		hover = true,
		documentSymbol = true,
		codeAction = true,
		completion = true
	},
	on_attach = function()
		vim.o.updatetime = 300
		vim.api.nvim_command [[autocmd! User LspDiagnosticsChanged lua vim.lsp.diagnostic.set_loclist({open_loclist = false})]]
		vim.api.nvim_command [[autocmd CursorHold * lua vim.lsp.diagnostic.show_line_diagnostics()]]
		on_attach()
	end,
}
-- VSCode LSPs need some fake settings to work:
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
local vscode_lsps = {
	'cssls',
	'html',
}
for _, lsp in ipairs(vscode_lsps) do
	lspconfig[lsp].setup {
		on_attach = on_attach,
		handlers = no_diagnostics,
		capabilities = capabilities,
	}
end
lspconfig.jsonls.setup {
	on_attach = on_attach,
	capabilities = capabilities,
	handlers = no_diagnostics,
	cmd = {'json-languageserver', '--stdio'},
}
