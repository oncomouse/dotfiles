-- luacheck: globals vim
local lspconfig = require('lspconfig')
-- vim.lsp.set_log_level('debug')

-- Disable diagnostics
local handler_no_diagnostics = {
	["textDocument/publishDiagnostics"] = function() end,
	["textDocument/formatting"] = function() end,
}

local handler_diagnostics = {
	["textDocument/publishDiagnostics"] = vim.lsp.with(
		vim.lsp.diagnostic.on_publish_diagnostics, {
		signs = false,
		underline = true,
		update_in_insert = false,
		virtual_text = false,
		}
	)
}

local vscode_capabilities = vim.lsp.protocol.make_client_capabilities()
vscode_capabilities.textDocument.completion.completionItem.snippetSupport = true

local on_attach = function(_, bufnr)
	-- Once codelens is setup:
	-- vim.api.nvim_command [[autocmd CursorHold,CursorHoldI,InsertLeave <buffer> lua vim.lsp.codelens.refresh()]]
	vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
	vim.api.nvim_command("call dotfiles#autocomplete#nvim_lsp#attach()")
	-- Load fzf-lsp, if we have access to FZF:
	if vim.fn.executable('fzf') then
		vim.g.fzf_lsp_preview_window = {'right:40%', 'ctrl-/'}
		require'fzf_lsp'.setup()
	end
end

local on_attach_diagnostics = function(x, bufnr)
	vim.o.updatetime = 300
	vim.api.nvim_command [[autocmd! User LspDiagnosticsChanged lua vim.lsp.diagnostic.set_loclist({open_loclist = false})]]
	vim.api.nvim_command [[autocmd CursorHold * lua vim.lsp.diagnostic.show_line_diagnostics()]]
	on_attach(x, bufnr)
end

local servers = {
	efm = {
		root_dir =  lspconfig.util.root_pattern("Rakefile", "yarn.lock", "lerna.json", ".git", "poetry.toml"),
		filetypes = {
			'css',
			'html',
			'javascript',
			'json',
			'lua',
			'markdown',
			'python',
			'sh',
			'vim',
		},
		init_options = {
			documentFormatting = true,
			hover = true,
			documentSymbol = true,
			codeAction = true,
			completion = true
		},
		diagnostics = true,
	},
	sumneko_lua ={
		cmd = {'sumneko-lua-language-server'},
		settings ={
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
		},
	},
	citation_lsp ={
		settings = {
			citation = {
				bibliographies = {
					'~/Seadrive/My Libraries/My Library/Documents/Academic Stuff/library.bib'
				}
			}
		}
	},
	cssls = {
		capabilities = vscode_capabilities,
	},
	html = {
		capabilities = vscode_capabilities,
	},
	jsonls = {
		capabilities = vscode_capabilities,
		cmd = {'json-languageserver', '--stdio'},
	},
	solargraph = {
		diagnostics = true
	},
	vimls = {},
	bashls ={},
	pyls ={},
	tsserver ={},
}
for lsp, settings in pairs(servers) do
	local tbl = {
		on_attach = on_attach,
		handlers = handler_no_diagnostics,
	}
	if #vim.tbl_keys(settings) > 0 then
		if settings.diagnostics ~= nil then
			tbl = {
				on_attach = on_attach_diagnostics,
				handlers = handler_diagnostics,
			}
		end
		tbl = vim.tbl_extend("keep", tbl, settings)
	end
	lspconfig[lsp].setup(tbl)
end
