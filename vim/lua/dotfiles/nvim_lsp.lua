-- luacheck: globals vim
local nvim_lsp = require"nvim_lsp"
nvim_lsp.cssls.setup{}
nvim_lsp.html.setup{}
nvim_lsp.jedi_language_server.setup{}
nvim_lsp.jsonls.setup{}
nvim_lsp.solargraph.setup{}
nvim_lsp.vimls.setup{}
nvim_lsp.citation_langserver.setup{
	settings = {
		citation = {
			bibliographies = { vim.g.bibliography_file },
		},
	},
}
