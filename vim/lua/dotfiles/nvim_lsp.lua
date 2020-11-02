local lsp = require'nvim_lsp'
local configs = require'nvim_lsp/configs'
local util = require'nvim_lsp/util'
-- let g:LanguageClient_serverCommands = {
-- 	  \ 'python': ['/usr/local/bin/jedi-language-server'],
-- 	  \ 'ruby': ['~/.asdf/shims/solargraph', 'stdio'],
-- 	  \ 'html': ['/usr/local/bin/html-languageserver', '--stdio'],
-- 	  \ 'scss': ['/usr/local/bin/css-languageserver', '--stdio'],
-- 	  \ 'css': ['/usr/local/bin/css-languageserver', '--stdio'],
-- 	  \ 'json': ['/usr/local/bin/json-languageserver', '--stdio'],
-- 	  \ 'lua': ['/usr/bin/java', '-cp', '~/dotfiles/lsp/EmmyLua-LS-all.jar', 'com.tang.vscode.MainKt'],
-- 	  \ 'vim': ['/usr/local/bin/vim-language-server', '--stdio'],
-- 	  \ 'markdown': ['/usr/local/bin/citation-langserver'],
-- 	  \}

configs.jedi_lsp = {
	default_config = {
		cmd = {"/usr/local/bin/jedi-language-server"},
		filetypes ={"python"},
		root_dir = util.root_pattern("requirements.txt", "pyproject.toml", "Pipfile", ".git"),
	}
}

configs.citation_langserver = {
	default_config = {
		cmd = {"/usr/local/bin/citation-langserver"},
		filetypes = {"markdown"},
		root_dir = util.path.dirname,
		settings = {
			citation = {
				bibliographies = vim.api.nvim_get_var("bibliography_file"),
			}
		}
	}
}

configs.emmy_lua = {
	default_config = {
		cmd = {"/usr/bin/java", "-cp ~/dotfiles/lsp/EmmyLua-LS-all.jar", "com.tang.vscode.MainKt"},
		filetypes = {"lua"},
		root_dir = util.path.dirname,
	}
}

local active_configs = {
    lsp.jedi_lsp,
    lsp.citation_langserver,
	lsp.tsserver,
	lsp.vimls,
	lsp.solargraph,
	lsp.html,
	lsp.cssls,
	lsp.jsonls,
	lsp.emmy_lua,
}

for i,config in pairs(active_configs) do
	config.setup({})
end
