-- luacheck: globals vim dotfiles
dotfiles = _G.dotfiles or {}
local lspconfig = require("lspconfig")
local configs = require("lspconfig/configs")
local util = require("lspconfig/util")
local map = require("dotfiles.utils.map")

local function show_documentation()
	if vim.tbl_contains({"vim", "help"}, vim.opt.filetype:get()) then
		vim.api.nvim_command("h " .. vim.fn.expand("<cword>"))
	else
		vim.lsp.buf.hover()
	end
end
map.nnoremap("<silent>", "K", function() show_documentation() end)

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
			update_in_insert = true,
			virtual_text = false,
		}
	)
}

local vscode_capabilities = vim.lsp.protocol.make_client_capabilities()
vscode_capabilities.textDocument.completion.completionItem.snippetSupport = true

local on_attach = function(client, _)
	-- Once codelens is setup:
	if client.resolved_capabilities.code_lens then
		vim.api.nvim_command [[autocmd CursorHold,CursorHoldI,InsertLeave <buffer> lua vim.lsp.codelens.refresh()]]
	end
	vim.opt_local.omnifunc = "v:lua.vim.lsp.omnifunc"
	map.nnoremap("<silent><buffer>", "<leader>s", function() vim.lsp.buf.document_symbol() end)
	map.nnoremap("<silent><buffer>", "<F2>", function() vim.lsp.buf.rename() end)
	map.nnoremap("<silent><buffer>", "gd", function() vim.lsp.buf.definition() end)
	map.nnoremap("<silent><buffer>", "gy", function() vim.lsp.buf.type_definition() end)
	map.nnoremap("<silent><buffer>", "gi", function() vim.lsp.buf.implementation() end)
	map.nnoremap("<silent><buffer>", "gr", function() vim.lsp.buf.references() end)
	map.nnoremap("<silent><buffer>", "gl", function() vim.lsp.codelens.run() end)
	map.nnoremap("<silent><buffer>", "ga", function() vim.lsp.buf.code_action() end)
	map.vnoremap("<silent><buffer>", "ga", function() vim.lsp.buf.range_code_action() end)
	map.nnoremap("<silent><buffer>", "<F5>", ":<CR>")
	-- vim.api.nvim_command("command! Format lua vim.lsp.buf.formatting()")
	-- vim.opt.wrapscan = true
	-- map.nnoremap("<silent><buffer>", "]d", function() vim.lsp.diagnostic.goto_next() end)
	-- map.nnoremap("<silent><buffer>", "[d", function() vim.lsp.diagnostic.goto_prev() end)
end

-- local on_attach_diagnostics = function(...)
-- 	vim.opt.updatetime = 300
-- 	vim.cmd[[autocmd! User LspDiagnosticsChanged lua vim.lsp.diagnostic.set_loclist({open_loclist = false})]]
-- 	vim.cmd[[autocmd! CursorMoved,CursorHold,InsertLeave * lua vim.lsp.diagnostic.show_line_diagnostics({ focusable = false, show_header = false, })]]
-- 	on_attach(...)
-- end

local servers = {
	-- efm = {
	-- 	root_dir =	lspconfig.util.root_pattern(
	-- 		"Rakefile",
	-- 		"yarn.lock",
	-- 		"lerna.json",
	-- 		".git",
	-- 		"poetry.toml"
	-- 	),
	-- 	filetypes = {
	-- 		"css",
	-- 		"html",
	-- 		"javascript",
	-- 		"json",
	-- 		"lua",
	-- 		"python",
	-- 		"scss",
	-- 		"sh",
	-- 		"vim",
	-- 		"yaml",
	-- 	},
	-- 	init_options = {
	-- 		documentFormatting = true,
	-- 		hover = true,
	-- 		documentSymbol = true,
	-- 		codeAction = true,
	-- 		completion = true
	-- 	},
	-- 	diagnostics = true,
	-- },
	sumneko_lua ={
		cmd = {"sumneko-lua-language-server"},
		settings ={
			Lua = {
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
		autostart = false,
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
		diagnostics = false
	},
	vimls = {},
	bashls = {},
	pyright = {},
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
