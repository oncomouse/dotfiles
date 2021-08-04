-- luacheck: globals vim dotfiles
local lspconfig = require("lspconfig")
local map = require("dotfiles.utils.map")

local function show_documentation()
	if vim.tbl_contains({ "vim", "help" }, vim.opt.filetype:get()) then
		vim.api.nvim_command("h " .. vim.fn.expand("<cword>"))
	else
		vim.lsp.buf.hover()
	end
end

vim.cmd([[
	packadd plenary.nvim
	packadd null-ls.nvim
]])

local FORMATTING = require("null-ls.methods").internal.FORMATTING
local RANGE_FORMATTING = require("null-ls.methods").internal.RANGE_FORMATTING
local get_prettier_generator_args = function(common_args)
	return function(params)
		local args = vim.deepcopy(common_args)

		if params.method == FORMATTING then
			return args
		end

		local content, range = params.content, params.range

		local row, col = range.row, range.col
		local range_start = row == 1 and 0
			or vim.fn.strchars(table.concat({ unpack(content, 1, row - 1) }, "\n") .. "\n", true)
		range_start = range_start + vim.fn.strchars(vim.fn.strcharpart(unpack(content, row, row), 0, col), true)

		local end_row, end_col = range.end_row, range.end_col
		local range_end = end_row == 1 and 0
			or vim.fn.strchars(table.concat({ unpack(content, 1, end_row - 1) }, "\n") .. "\n", true)
		range_end = range_end + vim.fn.strchars(vim.fn.strcharpart(unpack(content, end_row, end_row), 0, end_col), true)

		table.insert(args, "--range-start")
		table.insert(args, range_start)
		table.insert(args, "--range-end")
		table.insert(args, range_end)

		return args
	end
end
require("null-ls").config({
	sources = {
		require("null-ls").builtins.formatting.stylua,
		require("null-ls").builtins.formatting.black,
		require("null-ls.helpers").make_builtin({
			method = { FORMATTING, RANGE_FORMATTING },
			filetypes = {
				"javascript",
				"javascriptreact",
				"typescript",
				"typescriptreact",
				"vue",
				"svelte",
				"css",
				"scss",
				"html",
				"json",
				"yaml",
				"markdown",
			},
			generator_opts = {
				command = "prettier",
				args = get_prettier_generator_args({ "--use-tabs", "--stdin-filepath", "$FILENAME" }),
				to_stdin = true,
			},
			factory = require("null-ls.helpers").formatter_factory,
		}),
		require("null-ls").builtins.formatting.shfmt,
		require("null-ls").builtins.formatting.rufo,
	},
})

local vscode_capabilities = vim.lsp.protocol.make_client_capabilities()
vscode_capabilities.textDocument.completion.completionItem.snippetSupport = true

local on_attach = function(client, _)
	-- Once codelens is setup:
	if client.resolved_capabilities.code_lens then
		vim.api.nvim_command([[autocmd CursorHold,CursorHoldI,InsertLeave <buffer> lua vim.lsp.codelens.refresh()]])
	end
	-- Disable diagnostics
	client.resolved_capabilities.diagnostics = false
	vim.opt_local.omnifunc = "v:lua.vim.lsp.omnifunc"
	map.nnoremap("<silent><buffer>", "<leader>s", function()
		vim.lsp.buf.document_symbol()
	end)
	map.nnoremap("<silent><buffer>", "<F2>", function()
		vim.lsp.buf.rename()
	end)
	map.nnoremap("<silent><buffer>", "gd", function()
		vim.lsp.buf.definition()
	end)
	map.nnoremap("<silent><buffer>", "gy", function()
		vim.lsp.buf.type_definition()
	end)
	map.nnoremap("<silent><buffer>", "gi", function()
		vim.lsp.buf.implementation()
	end)
	map.nnoremap("<silent><buffer>", "gr", function()
		vim.lsp.buf.references()
	end)
	map.nnoremap("<silent><buffer>", "gl", function()
		vim.lsp.codelens.run()
	end)
	map.nnoremap("<silent><buffer>", "ga", function()
		vim.lsp.buf.code_action()
	end)
	map.vnoremap("<silent><buffer>", "ga", function()
		vim.lsp.buf.range_code_action()
	end)
	map.nnoremap("<silent><buffer>", "K", function()
		show_documentation()
	end)
	map.nnoremap("<silent><buffer>", "<F5>", ":<CR>")
	-- Set snippet integration:
	if vim.tbl_contains({ "html", "jsonls", "cssls" }, client.name) then
		vim.cmd([[
			packadd vim-vsnip
			packadd vim-vsnip-integ
		]])
		map.imap(
			"<expr><buffer>",
			"<C-l>",
			"vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<Plug>(dotfiles-lexima)'"
		)
		map.smap(
			"<expr><buffer>",
			"<C-l>",
			"vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<Plug>(dotfiles-lexima)'"
		)
	end
	-- Formatting:
	if client.name == "null-ls" then
		vim.cmd([[command! Format lua vim.lsp.buf.formatting()]])
	else
		client.resolved_capabilities.document_formatting = false
	end
end

local servers = {
	sumneko_lua = {
		cmd = { "sumneko-lua-language-server" },
		settings = {
			Lua = {
				runtime = {
					version = "LuaJIT",
					path = vim.split(package.path, ";"),
				},
				workspace = {
					library = {
						[vim.fn.expand("$VIMRUNTIME/lua")] = true,
						[vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
					},
				},
			},
		},
	},
	cssls = {
		capabilities = vscode_capabilities,
		cmd = { "css-languageserver", "--stdio" },
	},
	html = {
		capabilities = vscode_capabilities,
		cmd = { "html-languageserver", "--stdio" },
	},
	jsonls = {
		capabilities = vscode_capabilities,
		cmd = { "json-languageserver", "--stdio" },
	},
	solargraph = {},
	vimls = {},
	bashls = {},
	pyright = {},
	tsserver = {},
	["null-ls"] = {},
}
for lsp, settings in pairs(servers) do
	local tbl = {
		on_attach = on_attach,
	}
	if #vim.tbl_keys(settings) > 0 then
		tbl = vim.tbl_extend("keep", tbl, settings)
	end
	lspconfig[lsp].setup(tbl)
end
