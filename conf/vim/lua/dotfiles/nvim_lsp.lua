-- luacheck: globals vim dotfiles
local lspconfig = require("lspconfig")
local map = require("dotfiles.utils.map")

local diagnostics_providers = {
	'null-ls',
	'cssls',
	'jsonls',
	'solargraph',
}

local function show_documentation()
	if vim.tbl_contains({ "vim", "help" }, vim.opt.filetype:get()) then
		vim.api.nvim_command("h " .. vim.fn.expand("<cword>"))
	else
		vim.lsp.buf.hover()
	end
end

require("dotfiles.null_ls")

local vscode_capabilities = vim.lsp.protocol.make_client_capabilities()
vscode_capabilities.textDocument.completion.completionItem.snippetSupport = true
local handler_no_diagnostics = {
	["textDocument/publishDiagnostics"] = function() end,
	["textDocument/formatting"] = function() end,
}

local on_attach = function(client, _)
	-- Once codelens is setup:
	if client.resolved_capabilities.code_lens then
		vim.api.nvim_command([[autocmd CursorHold,CursorHoldI,InsertLeave <buffer> lua vim.lsp.codelens.refresh()]])
	end
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
			packadd LuaSnip
		]])
	end
	if vim.tbl_contains(diagnostics_providers, client.name) then
		if vim.diagnostic ~= nil then -- Neovim 0.6:
			vim.cmd([[
				autocmd! dotfiles-settings User DiagnosticsChanged lua vim.diagnostic.setloclist({ open = false })
			]])
			map.nnoremap("<silent><buffer>", "]d", function() vim.diagnostic.goto_next() end)
			map.nnoremap("<silent><buffer>", "[d", function() vim.diagnostic.goto_prev() end)
		else -- Neovim 0.5:
			vim.cmd([[
				autocmd! dotfiles-settings User LspDiagnosticsChanged lua vim.lsp.diagnostic.set_loclist({ open = false })
			]])
			map.nnoremap("<silent><buffer>", "]d", function() vim.lsp.diagnostic.goto_next() end)
			map.nnoremap("<silent><buffer>", "[d", function() vim.lsp.diagnostic.goto_prev() end)
		end
		end
	-- Formatting:
	if vim.tbl_contains({ 'null-ls', 'solargraph' }, client.name) then
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
	['null-ls'] = {},
}
for lsp, settings in pairs(servers) do
	local tbl = {
		on_attach = on_attach,
	}
	if #vim.tbl_keys(settings) > 0 then
		tbl = vim.tbl_extend("keep", tbl, settings)
	end
	if not vim.tbl_contains(diagnostics_providers, lsp) then
		tbl.handlers = handler_no_diagnostics
	end
	lspconfig[lsp].setup(tbl)
end
lspconfig['null-ls'].setup({
	on_attach = on_attach,
})
