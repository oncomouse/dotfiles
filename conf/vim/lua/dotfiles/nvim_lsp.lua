-- luacheck: globals vim
local lspconfig = require('lspconfig')
-- vim.lsp.set_log_level('debug')

-- Disable diagnostics
vim.lsp.handlers["textDocument/publishDiagnostics"] = function() end

local on_attach = function(_, bufnr)
	-- Once codelens is setup:
	-- vim.api.nvim_command [[autocmd CursorHold,CursorHoldI,InsertLeave <buffer> lua vim.lsp.codelens.refresh()]]
	vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
end

local servers = {
	'tsserver',
	'vimls',
	'jedi_language_server',
	'solargraph' ,
	'bashls',
}
for _, lsp in ipairs(servers) do
	lspconfig[lsp].setup {
	on_attach = on_attach,
	}
end

lspconfig['sumneko_lua'].setup{
	on_attach = on_attach,
	cmd = {'sumneko-lua-language-server'}
}

lspconfig['citation_lsp'].setup{
	on_attach = on_attach,
	settings = {
		citation = {
			bibliographies = {
			'~/Seadrive/My Libraries/My Library/Documents/Academic Stuff/library.bib'
			}
		}
	}
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
	capabilities = capabilities,
	}
end
lspconfig['jsonls'].setup {
	on_attach = on_attach,
	capabilities = capabilities,
	cmd = {'json-languageserver', '--stdio'},
}
