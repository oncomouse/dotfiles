-- luacheck: globals vim
local lspconfig = require('lspconfig')
-- vim.lsp.set_log_level('debug')

-- Disable diagnostics
-- vim.lsp.handlers["textDocument/publishDiagnostics"] = function() end

local no_diagnostics = {
	["textDocument/publishDiagnostics"] = function() end
}

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
		handlers = no_diagnostics,
	}
end

lspconfig.sumneko_lua.setup{
	on_attach = on_attach,
	cmd = {'sumneko-lua-language-server'},
	handlers = no_diagnostics,
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

local html_hint = require('dotfiles.efm-lsp.html-hint')
local html_beautify = require('dotfiles.efm-lsp.html-beautify')
local semistandard = require('dotfiles.efm-lsp.semistandard')
local luacheck = require('dotfiles.efm-lsp.luacheck')
local prettier = require('dotfiles.efm-lsp.prettier')
local yamllint = require('dotfiles.efm-lsp.yamllint')
local jq = require('dotfiles.efm-lsp.jq')
local vint = {
    lintCommand = 'vint -',
    lintStdin = true,
    lintFormats = { '%f:%l:%c: %m' },
}
local efm_languages = {
	lua = {luacheck},
	html = {html_hint, html_beautify},
	yaml = {prettier, yamllint},
	typescript = {semistandard},
	javascript = {semistandard},
	typescriptreact = {semistandard},
	javascriptreact = {semistandard},
	json = {prettier, jq},
	scss = {prettier},
	css = {prettier},
	vim = {vint},
	-- markdown = {prettier}
}
lspconfig.efm.setup {
    root_dir = lspconfig.util.root_pattern(
		"yarn.lock",
		"lerna.json",
		".git"
	),
    filetypes = vim.tbl_keys(efm_languages),
    init_options = {documentFormatting = true, codeAction = true},
    settings = {languages = efm_languages, log_level = 1, log_file = '~/.cache/nvim/efm.log'},
    on_attach = on_attach
}
