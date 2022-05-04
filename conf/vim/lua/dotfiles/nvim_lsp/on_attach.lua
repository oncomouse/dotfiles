local servers = require("dotfiles.nvim_lsp.servers")
local function on_attach(client, buf_num)
	-- Update codeLens:
	if client.server_capabilities.codeLensProvider then
		vim.api.nvim_create_autocmd("CursorHold,CursorHoldI,InsertLeave", {
			buffer = buf_num,
			group = "dotfiles-settings",
			callback = function()
				vim.lsp.codelens.refresh()
			end,
		})
	end
	-- Use C+x C+o for completion:
	vim.opt_local.omnifunc = "v:lua.vim.lsp.omnifunc"
	vim.keymap.set("n", "<leader>s", vim.lsp.buf.document_symbol, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.document_symbol()",
	})
	vim.keymap.set("n", "<F2>", vim.lsp.buf.rename, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.rename()",
	})
	vim.keymap.set("n", "<leader>gd", vim.lsp.buf.definition, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.definition()",
	})
	-- if client.server_capabilities.gotoDefinitionProvider == true then
	vim.opt_local.tagfunc = "v:lua.vim.lsp.tagfunc"
	-- end
	vim.keymap.set("n", "<leader>gD", vim.lsp.buf.declaration, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.declaration()",
	})
	vim.keymap.set("n", "<leader>gy", vim.lsp.buf.type_definition, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.type_definition()",
	})
	vim.keymap.set("n", "<leader>gi", vim.lsp.buf.implementation, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.implementation()",
	})
	vim.keymap.set("n", "<leader>gr", vim.lsp.buf.references, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.references()",
	})
	vim.keymap.set("n", "<leader>gl", vim.lsp.codelens.run, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.codelens.run()",
	})
	vim.keymap.set("n", "<leader>ga", vim.lsp.buf.code_action, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.code_action()",
	})
	vim.keymap.set("v", "<leader>ga", vim.lsp.buf.range_code_action, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.range_code_action()",
	})
	vim.keymap.set("n", "K", require("dotfiles.nvim_lsp.show_documentation"), {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua require('dotfiles.nvim_lsp.show_documentation')()",
	})
	vim.keymap.set("n", "<C-k>", vim.lsp.buf.signature_help, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.signature_help()",
	})
	local snippet_provider = vim.tbl_contains(servers[client.name].provides or {}, "snippets")
	local diagnostic_provider = vim.tbl_contains(servers[client.name].provides or {}, "diagnostics")
	local formatting_provider = vim.tbl_contains(servers[client.name].provides or {}, "formatting")
	if snippet_provider then
		vim.cmd([[packadd vim-vsnip-integ]])
	end
	if diagnostic_provider then
		vim.api.nvim_create_autocmd("DiagnosticChanged,BufEnter", {
			buffer = buf_num,
			group = "dotfiles-settings",
			callback = function()
				vim.diagnostic.setloclist({ open = false })
			end,
		})
		vim.keymap.set("n", "]d", vim.diagnostic.goto_next, {
			silent = true,
			noremap = true,
			buffer = true,
			desc = "lua vim.diagnostic.goto_next()",
		})
		vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, {
			silent = true,
			noremap = true,
			buffer = true,
			desc = "lua vim.diagnostic.goto_prev()",
		})
	end
	-- Formatting:
	if formatting_provider then
		vim.opt_local.formatexpr = "v:lua.vim.lsp.formatexpr()"
		vim.api.nvim_buf_create_user_command(buf_num, "Format", function()
			vim.lsp.buf.format({ async = true })
		end, {
			desc = "lua vim.lsp.buf.formatting()",
			force = true,
		})
	else
		client.server_capabilities.documentFormattingProvider = false
		client.server_capabilities.documentRangeFormattingProvider = false
	end
end

return on_attach
