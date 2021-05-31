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
			update_in_insert = true,
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

local function in_range(range, line, col)
	local in_start = range.start.line < line or (range.start.line == line and range.start.character < col)
	local in_end = range["end"].line > line or (range["end"].line == line and range["end"].character >= col)
	return in_start and in_end
end

-- local function clone_function(fn)
-- 	local dumped = string.dump(fn)
-- 	local cloned = loadstring(dumped)
-- 	local i = 1
-- 	while true do
-- 		local name = debug.getupvalue(fn, i)
-- 		if not name then
-- 			break
-- 		end
-- 		debug.upvaluejoin(cloned, i, fn, i)
-- 		i = i + 1
-- 	end
-- 	return cloned
-- end

local echoed = false

-- Overwrite the original show_line_diagnostics to output in modeline and to be cursor aware:
vim.lsp.diagnostic.show_line_diagnostics = function(opts, bufnr, line_nr, client_id)
	opts = opts or {}

	local show_header = vim.F.if_nil(opts.show_header, true)

	bufnr = bufnr or 0
	line_nr = line_nr or (vim.api.nvim_win_get_cursor(0)[1] - 1)
	col_nr = vim.fn.col(".")

	local lines = {}
	local highlights = {}
	if show_header then
		table.insert(lines, "Diagnostics:")
		table.insert(highlights, {0, "Bold"})
	end

	local line_diagnostics = vim.tbl_filter(
		function(x) return in_range(x.range, line_nr, col_nr) end,
		vim.lsp.diagnostic.get_line_diagnostics(bufnr, line_nr, opts, client_id)
	)
	if vim.tbl_isempty(line_diagnostics) then
		if echoed then
			vim.cmd("echo ''")
			echoed = false
		end
		return
	end

	-- Echo if only one diagnostic message and it's short:
	if #line_diagnostics == 1 and
	#vim.split(line_diagnostics[1].message, '\n', true) == 1 and
	string.len(line_diagnostics[1].message) < vim.fn.winwidth(".") then
		echoed = true
		vim.cmd("echo \"" .. line_diagnostics[1].message .. "\"")
		return
	end

	for i, diagnostic in ipairs(line_diagnostics) do
		local prefix = string.format("%d. ", i)
		local hiname = vim.lsp.diagnostic._get_floating_severity_highlight_name(diagnostic.severity)
		assert(hiname, 'unknown severity: ' .. tostring(diagnostic.severity))

		local message_lines = vim.split(diagnostic.message, '\n', true)
		table.insert(lines, prefix..message_lines[1])
		table.insert(highlights, {#prefix, hiname})
		for j = 2, #message_lines do
			table.insert(lines, message_lines[j])
			table.insert(highlights, {0, hiname})
		end
	end

	if #lines > 0 then
		local popup_bufnr, winnr = vim.lsp.util.open_floating_preview(lines, 'plaintext', opts)
		for i, hi in ipairs(highlights) do
			local prefixlen, hiname = unpack(hi)
			-- Start highlight after the prefix
			vim.api.nvim_buf_add_highlight(popup_bufnr, -1, hiname, i-1, prefixlen, -1)
		end
		return popup_bufnr, winnr
	end
end

local on_attach_diagnostics = function(...)
	vim.opt.updatetime = 300
	vim.api.nvim_command("autocmd! User LspDiagnosticsChanged lua vim.lsp.diagnostic.set_loclist({open_loclist = false})")
	vim.api.nvim_command("autocmd CursorMoved,CursorMovedI * lua vim.lsp.diagnostic.show_line_diagnostics()")
	on_attach(...)
end

local servers = {
	efm = {
		root_dir =	lspconfig.util.root_pattern("Rakefile", "yarn.lock", "lerna.json", ".git", "poetry.toml"),
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
				--	enable = true,
				--	globals = {"vim"},
				--	disable = { "lowercase-global" },
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
