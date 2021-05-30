-- luacheck: globals vim
local lspconfig = require("lspconfig")
local configs = require("lspconfig/configs")
local util = require("lspconfig/util")
local map = require("dotfiles.utils.map")

function _G.show_documentation()
	if vim.tbl_contains({"vim", "help"}, vim.bo.filetype) then
		vim.api.nvim_command("h " .. vim.fn.expand("<cword>"))
	else
		vim.lsp.buf.hover()
	end
end
map.nnoremap("<silent>", "<Plug>(dotfiles-documentation)", ":<C-u>call v:lua.show_documentation()<CR>")

-- Extend configs for citation-langserver:
configs.citation_langserver = {
  default_config = {
	filetypes = {"markdown", "pandoc"};
	root_dir = function(fname)
	  return util.find_git_ancestor(fname) or util.path.dirname(fname)
	end;
	log_level = vim.lsp.protocol.MessageType.Warning;
	cmd = {"/usr/bin/env", "citation-langserver"};
	-- cmd = {"env", "PYTHONPATH=~/Projects/citation-langserver", "python3", "-m", "citation_langserver"}
  }
}

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

local on_attach = function()
	-- Once codelens is setup:
	-- vim.api.nvim_command [[autocmd CursorHold,CursorHoldI,InsertLeave <buffer> lua vim.lsp.codelens.refresh()]]
	vim.opt_local.omnifunc = "v:lua.vim.lsp.omnifunc"
	map.nnoremap("<silent><buffer>", "<Plug>(dotfiles-document-symbols)", ":<C-u>lua vim.lsp.buf.document_symbol()<CR>")
	map.nnoremap("<silent><buffer>", "<Plug>(dotfiles-rename)", ":<C-u>lua vim.lsp.buf.rename()<CR>")
	map.nnoremap("<silent><buffer>", "<Plug>(dotfiles-definition)", ":<C-u>lua vim.lsp.buf.definition()<CR>")
	map.nnoremap("<silent><buffer>", "<Plug>(dotfiles-type-definition)", ":<C-u>lua vim.lsp.buf.type_definition()<CR>")
	map.nnoremap("<silent><buffer>", "<Plug>(dotfiles-implementation)", ":<C-u>lua vim.lsp.buf.implementation()<CR>")
	map.nnoremap("<silent><buffer>", "<Plug>(dotfiles-references)", ":<C-u>lua vim.lsp.buf.references()<CR>")
	map.nnoremap("<silent><buffer>", "<Plug>(dotfiles-codelens)", ":<C-u>lua vim.lsp.codelens.run()<CR>")
	map.nnoremap("<silent><buffer>", "<Plug>(dotfiles-codeaction)", ":<C-u>lua vim.lsp.buf.code_action()<CR>")
	map.vnoremap("<silent><buffer>", "<Plug>(dotfiles-codeaction-selected)", ":<C-u>lua vim.lsp.buf.range_code_action()<CR>")
	map.nnoremap("<silent><buffer>", "<Plug>(dotfiles-commands)", ":<CR>")
	vim.api.nvim_command("command! Format lua vim.lsp.buf.formatting()")
	vim.opt.wrapscan = true
	map.nnoremap("<silent><buffer>", "<Plug>(dotfiles-diagnostic-next)", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>")
	map.nnoremap("<silent><buffer>", "<Plug>(dotfiles-diagnostic-previous)", "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>")
end

-- Show diagnostics works like LanguageClient-neovim now:
-- Cache the default function:
local diag = loadstring(string.dump(vim.lsp.diagnostic.show_line_diagnostics))
vim.lsp.diagnostic.show_line_diagnostics = function()
	local d = vim.lsp.diagnostic.get_line_diagnostics(vim.fn.bufnr("."), vim.fn.line(".") -1 )
	if #d == 0 then
		vim.cmd("echo ''")
	elseif #d == 1 and string.len(d[1].message) < vim.fn.winwidth(".") then
		vim.cmd("echo \"" .. d[1].message .. "\"")
	else
		diag()
	end
end

local on_attach_diagnostics = function(...)
	vim.opt.updatetime = 300
	vim.api.nvim_command("autocmd! User LspDiagnosticsChanged lua vim.lsp.diagnostic.set_loclist({open_loclist = false})")
	vim.api.nvim_command("autocmd CursorHold * lua vim.lsp.diagnostic.show_line_diagnostics()")
	on_attach(...)
end

local servers = {
	efm = {
		root_dir =  lspconfig.util.root_pattern("Rakefile", "yarn.lock", "lerna.json", ".git", "poetry.toml"),
		filetypes = {
			"css",
			"html",
			"javascript",
			"json",
			"lua",
			"markdown",
			"python",
			"sh",
			"vim",
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
		cmd = {"sumneko-lua-language-server"},
		settings ={
			Lua = {
				-- diagnostics = {
				-- 	enable = true,
				-- 	globals = {"vim"},
				-- 	disable = { "lowercase-global" },
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
	citation_langserver ={
		settings = {
			citation = {
				bibliographies = {
					"~/Seadrive/My Libraries/My Library/Documents/Academic Stuff/library.bib"
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
		cmd = {"json-languageserver", "--stdio"},
	},
	solargraph = {
		diagnostics = true
	},
	vimls = {},
	bashls ={},
	pyright ={},
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
