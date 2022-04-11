local servers = require("dotfiles.nvim_lsp.servers")
local function on_attach(client, buf_num)
	-- Update codeLens:
	if client.resolved_capabilities.code_lens then
		vim.api.nvim_create_autocmd("CursorHold,CursorHoldI,InsertLeave", {
			buffer = buf_num,
			group = "dotfiles-settings",
			callback = function()
				vim.lsp.codelens.refresh()
			end,
		})
	end
	-- Use C+x C+o for completion:
	-- vim.opt_local.omnifunc = "v:lua.vim.lsp.omnifunc"
	vim.keymap.set("n", "<leader>s", function()
		vim.lsp.buf.document_symbol()
	end, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.document_symbol()",
	})
	vim.keymap.set("n", "<F2>", function()
		vim.lsp.buf.rename()
	end, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.rename()",
	})
	vim.keymap.set("n", "<leader>gd", function()
		vim.lsp.buf.definition()
	end, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.definition()",
	})
	if client.resolved_capabilities.goto_definition == true then
		vim.opt_local.tagfunc = "v:lua.vim.lsp.tagfunc"
	end
	vim.keymap.set("n", "<leader>gD", function()
		vim.lsp.buf.declaration()
	end, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.declaration()",
	})
	vim.keymap.set("n", "<leader>gy", function()
		vim.lsp.buf.type_definition()
	end, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.type_definition()",
	})
	vim.keymap.set("n", "<leader>gi", function()
		vim.lsp.buf.implementation()
	end, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.implementation()",
	})
	vim.keymap.set("n", "<leader>gr", function()
		vim.lsp.buf.references()
	end, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.references()",
	})
	vim.keymap.set("n", "<leader>gl", function()
		vim.lsp.codelens.run()
	end, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.codelens.run()",
	})
	vim.keymap.set("n", "<leader>ga", function()
		vim.lsp.buf.code_action()
	end, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.code_action()",
	})
	vim.keymap.set("v", "<leader>ga", function()
		vim.lsp.buf.range_code_action()
	end, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua vim.lsp.buf.range_code_action()",
	})
	vim.keymap.set("n", "K", function()
		require("dotfiles.nvim_lsp.show_documentation")()
	end, {
		silent = true,
		noremap = true,
		buffer = true,
		desc = "lua require('dotfiles.nvim_lsp.show_documentation')()",
	})
	vim.keymap.set("n", "<C-k>", function()
		vim.lsp.buf.signature_help()
	end, {
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
			end
		})
		vim.keymap.set("n", "]d", function()
			vim.diagnostic.goto_next()
		end, {
			silent = true,
			noremap = true,
			buffer = true,
			desc = "lua vim.diagnostic.goto_next()",
		})
		vim.keymap.set("n", "[d", function()
			vim.diagnostic.goto_prev()
		end, {
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
			vim.lsp.buf.formatting()
		end, {
			desc = "lua vim.lsp.buf.formatting()",
			force = true,
		})
	else
		client.resolved_capabilities.document_formatting = false
		client.resolved_capabilities.document_range_formatting = false
	end
end

return on_attach
