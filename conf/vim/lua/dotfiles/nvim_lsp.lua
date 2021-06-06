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
map.nnoremap("<silent>", "<Plug>(dotfiles-documentation)", ":<C-u>lua show_documentation()<CR>")

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
	map.nnoremap("<silent><buffer>", "<leader>s", ":<C-u>lua vim.lsp.buf.document_symbol()<CR>")
	map.nnoremap("<silent><buffer>", "<F2>", ":<C-u>lua vim.lsp.buf.rename()<CR>")
	map.nnoremap("<silent><buffer>", "gd", ":<C-u>lua vim.lsp.buf.definition()<CR>")
	map.nnoremap("<silent><buffer>", "gy", ":<C-u>lua vim.lsp.buf.type_definition()<CR>")
	map.nnoremap("<silent><buffer>", "gi", ":<C-u>lua vim.lsp.buf.implementation()<CR>")
	map.nnoremap("<silent><buffer>", "gr", ":<C-u>lua vim.lsp.buf.references()<CR>")
	map.nnoremap("<silent><buffer>", "gl", ":<C-u>lua vim.lsp.codelens.run()<CR>")
	map.nnoremap("<silent><buffer>", "ga", ":<C-u>lua vim.lsp.buf.code_action()<CR>")
	map.vnoremap("<silent><buffer>", "ga", ":<C-u>lua vim.lsp.buf.range_code_action()<CR>")
	map.nnoremap("<silent><buffer>", "<F5>", ":<CR>")
	vim.api.nvim_command("command! Format lua vim.lsp.buf.formatting()")
	vim.opt.wrapscan = true
	map.nnoremap("<silent><buffer>", "]d", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>")
	map.nnoremap("<silent><buffer>", "[d", "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>")
end

local function in_range(range, line, col)
	local in_start = range.start.line < line or (range.start.line == line and range.start.character < col)
	local in_end = range["end"].line > line or (range["end"].line == line and range["end"].character >= col)
	return in_start and in_end
end

local function clone_function(fn)
	local dumped = string.dump(fn)
	local cloned = loadstring(dumped)
	local i = 1
	while true do
		local name = debug.getupvalue(fn, i)
		if not name then
			break
		end
		debug.upvaluejoin(cloned, i, fn, i)
		i = i + 1
	end
	return cloned
end

local function truncate(st, len)
	return string.len(st) > len and string.sub(st, 0, len - 1) .. "…" or st
end

local echoed = false

local function filter_line_diagnostics(bufnr, line_nr, col_nr, opts, client_id)
	return vim.tbl_filter(
		function(x) return in_range(x.range, line_nr, col_nr) end,
		vim.lsp.diagnostic.get_line_diagnostics(bufnr, line_nr, opts, client_id)
	)
end

-- Overwrite the original show_line_diagnostics to output in modeline and to be cursor aware:
_G.hover_diagnostics = clone_function(vim.lsp.diagnostic.show_line_diagnostics)
vim.lsp.diagnostic.show_line_diagnostics = function() end
function _G.modeline_diagnostics(opts, bufnr, line_nr, client_id)
	opts = opts or {}

	local show_header = vim.F.if_nil(opts.show_header, true)

	bufnr = bufnr or 0
	line_nr = line_nr or (vim.api.nvim_win_get_cursor(0)[1] - 1)
	local col_nr = vim.fn.col(".")

	local lines = {}
	local highlights = {}
	if show_header then
		table.insert(lines, "Diagnostics:")
		table.insert(highlights, {0, "Bold"})
	end

	local line_diagnostics = filter_line_diagnostics(bufnr, line_nr, col_nr, opts, client_id)
	if vim.tbl_isempty(line_diagnostics) then
		if echoed then
			vim.cmd("echo ''")
			echoed = false
		end
	else
		echoed = true
		vim.cmd("echo \"" ..
			string.format(
				"%d Error%s: %s",
				#line_diagnostics,
				(#line_diagnostics == 1 and "" or "s"),
				truncate(line_diagnostics[1].message, vim.fn.winwidth(".") - 2)
			)
			.. "\"")
	end
end

function _G.preview_diagnostics(opts, bufnr, line_nr, client_id)
	opts = opts or {focusable = false}

	local show_header = vim.F.if_nil(opts.show_header, true)

	bufnr = bufnr or 0
	line_nr = line_nr or (vim.api.nvim_win_get_cursor(0)[1] - 1)
	local col_nr = vim.fn.col(".")

	local line_diagnostics = filter_line_diagnostics(bufnr, line_nr, col_nr, opts, client_id)

	if vim.tbl_isempty(line_diagnostics) then return end

	local lines = {}
	local highlights = {}
	if show_header then
		table.insert(lines, "Diagnostics:")
		table.insert(highlights, {0, "Bold"})
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

	local popup_bufnr, winnr = vim.lsp.util.open_floating_preview(lines, 'plaintext', opts)
	for i, hi in ipairs(highlights) do
		local prefixlen, hiname = unpack(hi)
		-- Start highlight after the prefix
		vim.api.nvim_buf_add_highlight(popup_bufnr, -1, hiname, i-1, prefixlen, -1)
	end
	return popup_bufnr, winnr
end

local on_attach_diagnostics = function(...)
	vim.opt.updatetime = 300
	vim.cmd[[autocmd! User LspDiagnosticsChanged lua vim.lsp.diagnostic.set_loclist({open_loclist = false})]]
	vim.cmd[[autocmd! CursorMoved,CursorHold,InsertLeave * lua modeline_diagnostics()]]
	vim.cmd[[command! LspDetail lua preview_diagnostics()]]
	-- vim.cmd[[autocmd CursorHold * lua hover_diagnostics({focusable = false})]]
	on_attach(...)
end

local servers = {
	efm = {
		root_dir =	lspconfig.util.root_pattern(
			"Rakefile",
			"yarn.lock",
			"lerna.json",
			".git",
			"poetry.toml"
		),
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
