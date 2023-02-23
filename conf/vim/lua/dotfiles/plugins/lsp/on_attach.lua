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
	if client.server_capabilities.completionProvider then
		vim.opt_local.omnifunc = "v:lua.vim.lsp.omnifunc"
	end
	-- Use C+x C+] for tags:
	if client.server_capabilities.definitionProvider then
		vim.opt_local.tagfunc = "v:lua.vim.lsp.tagfunc"
	end
	-- Use K for hover (except in VimL):
	if vim.opt_local.ft:get() ~= "vim" then
		vim.api.nvim_buf_create_user_command(buf_num, "Hover", vim.lsp.buf.hover, {
			desc = "lua vim.lsp.buf.hover()",
			force = true,
			nargs = "+",
		})
		vim.opt_local.keywordprg = ":Hover"
	end
	-- LSP-specific maps:
	vim.keymap.set("n", "<leader>s", vim.lsp.buf.document_symbol, {
		buffer = buf_num,
		desc = "lua vim.lsp.buf.document_symbol()",
	})
	vim.keymap.set("n", "<F2>", vim.lsp.buf.rename, {
		buffer = buf_num,
		desc = "lua vim.lsp.buf.rename()",
	})
	vim.keymap.set("n", "<leader>gd", vim.lsp.buf.definition, {
		buffer = buf_num,
		desc = "lua vim.lsp.buf.definition()",
	})
	vim.keymap.set("n", "<leader>gD", vim.lsp.buf.declaration, {
		buffer = buf_num,
		desc = "lua vim.lsp.buf.declaration()",
	})
	vim.keymap.set("n", "<leader>gy", vim.lsp.buf.type_definition, {
		buffer = buf_num,
		desc = "lua vim.lsp.buf.type_definition()",
	})
	vim.keymap.set("n", "<leader>gi", vim.lsp.buf.implementation, {
		buffer = buf_num,
		desc = "lua vim.lsp.buf.implementation()",
	})
	vim.keymap.set("n", "<leader>gr", vim.lsp.buf.references, {
		buffer = buf_num,
		desc = "lua vim.lsp.buf.references()",
	})
	vim.keymap.set("n", "<leader>gl", vim.lsp.codelens.run, {
		buffer = buf_num,
		desc = "lua vim.lsp.codelens.run()",
	})
	vim.keymap.set("n", "<leader>ga", vim.lsp.buf.code_action, {
		buffer = buf_num,
		desc = "lua vim.lsp.buf.code_action()",
	})
	vim.keymap.set("v", "<leader>ga", function()
		local _, s_row, s_col, _ = unpack(vim.fn.getpos("v"))
		local _, e_row, e_col, _ = unpack(vim.fn.getcurpos())
		if vim.fn.mode() == "V" then
			s_col = 1
			e_col = #(vim.fn.getline(e_row))
		end
		vim.lsp.buf.code_action({
			range = {
				start = { s_row, s_col },
				["end"] = { e_row, e_col },
			},
		})
	end, {
		buffer = buf_num,
		desc = "'<,'>lua vim.lsp.buf.code_action()",
	})
	vim.keymap.set("n", "<M-k>", vim.lsp.buf.signature_help, {
		buffer = buf_num,
		desc = "lua vim.lsp.buf.signature_help()",
	})
	local diagnostic_provider =
		vim.tbl_contains(require("dotfiles.plugins.lsp.servers")[client.name].provides or {}, "diagnostics")
	local formatting_provider =
		vim.tbl_contains(require("dotfiles.plugins.lsp.servers")[client.name].provides or {}, "formatting")
	if diagnostic_provider then
		vim.api.nvim_create_autocmd("DiagnosticChanged,BufEnter", {
			buffer = buf_num,
			group = "dotfiles-settings",
			callback = function()
				vim.diagnostic.setloclist({ open = false })
			end,
		})
		vim.keymap.set("n", "]d", vim.diagnostic.goto_next, {
			buffer = buf_num,
			desc = "lua vim.diagnostic.goto_next()",
		})
		vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, {
			buffer = buf_num,
			desc = "lua vim.diagnostic.goto_prev()",
		})
	end
	-- Formatting:
	if formatting_provider then
		vim.api.nvim_buf_create_user_command(buf_num, "Format", function()
			vim.lsp.buf.format({
				async = true,
				filter = function(c)
					return vim.tbl_contains(
						require("dotfiles.plugins.lsp.servers")[c.name].provides or {},
						"formatting"
					)
				end,
			})
		end, {
			desc = "lua vim.lsp.buf.format()",
			force = true,
		})
	else
		client.server_capabilities.documentFormattingProvider = false
		client.server_capabilities.documentRangeFormattingProvider = false
	end
end

return on_attach
