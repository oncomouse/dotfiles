local lsp = require'nvim_lsp'
local configs = require'nvim_lsp/configs'
local util = require'nvim_lsp/util'

configs.citation_langserver = {
	default_config = {
		cmd = {"/usr/local/bin/citation-langserver"},
		filetypes = {"markdown"},
		root_dir = util.root_pattern('.git') or vim.loop.os_homedir(),
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
		root_dir = util.root_pattern('.git') or vim.loop.os_homedir(),
	}
}

configs.custom_html = {
    default_config = {
        cmd = {"html-languageserver", "--stdio"};
        filetypes = {"html"};
        root_dir = util.root_pattern("package.json") or vim.loop.os_homedir(),
        settings = {
			capabilities = {
				textDocument = {
					completion = {
						completionItem = {
							snippetSupport = true
						}
					}
				}
			}
		},
        init_options = {
            embeddedLanguages = { css= true, javascript= true },
            configurationSection = { 'html', 'css', 'javascript' },
        }
    },
}

configs.custom_cssls = {
    default_config = {
        cmd = {"css-languageserver", "--stdio"},
        filetypes = {"css", "scss", "less"},
        root_dir = util.root_pattern("package.json") or vim.loop.os_homedir(),
        settings = {
            css = { validate = true },
            scss = { validate = true },
            less = { validate = true },
            capabilities = {
                textDocument = {
                    completion = {
                        completionItem = {
                            snippetSupport = true
                        }
                    }
                }
            }
		}
	}
}

-- lsp.tsserver,
local active_configs = {
	lsp.citation_langserver, -- X
	lsp.custom_html, -- X
	lsp.hls, -- X
	lsp.emmy_lua, -- X
	lsp.custom_cssls, -- .css, .scss, .sass
	lsp.jedi_language_server, -- .py
	lsp.vimls, -- .vim
	lsp.solargraph, -- .rb
}

for i,config in pairs(active_configs) do
	config.setup({
        on_attach = function(client, buffer)
			vim.bo.omnifunc='v:lua.vim.lsp.omnifunc'
        end
	})
end
