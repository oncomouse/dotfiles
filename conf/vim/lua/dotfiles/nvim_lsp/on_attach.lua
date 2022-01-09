-- luacheck: globals vim dotfiles
local servers = require("dotfiles.nvim_lsp.servers")
local function on_attach(client, buf_num)
	-- Update codeLens:
	if client.resolved_capabilities.code_lens then
		vim.api.nvim_command(
			[[autocmd! dotfiles-settings CursorHold,CursorHoldI,InsertLeave <buffer> lua vim.lsp.codelens.refresh()]]
		)
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
		vim.cmd(
			[[ autocmd! dotfiles-settings DiagnosticChanged <buffer> lua vim.diagnostic.setloclist({ open = false }) ]]
		)
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
		vim.api.nvim_buf_add_user_command(buf_num, "Format", function()
			vim.lsp.buf.formatting()
		end, {
			desc = "lua vim.lsp.buf.formatting()",
			force = true,
		})
	else
		client.resolved_capabilities.document_formatting = false
		client.resolved_capabilities.document_range_formatting = false
	end
	if client.name == "tsserver" then
		local ts_utils = require("nvim-lsp-ts-utils")

		-- defaults
		ts_utils.setup({
			debug = false,
			disable_commands = false,
			enable_import_on_completion = true,

			-- import all
			import_all_timeout = 5000, -- ms
			import_all_priorities = {
				buffers = 4, -- loaded buffer names
				buffer_content = 3, -- loaded buffer content
				local_files = 2, -- git files or files with relative path markers
				same_file = 1, -- add to existing import statement
			},
			import_all_scan_buffers = 100,
			import_all_select_source = false,

			-- update imports on file move
			update_imports_on_move = true,
			require_confirmation_on_move = false,
			watch_dir = nil,
		})

		-- required to fix code action ranges and filter diagnostics
		ts_utils.setup_client(client)

		-- no default maps, so you may want to define some here
		vim.keymap.set("n", "<leader>gs", ":TSLspOrganize<CR>", { buffer = true, silent = true })
		vim.keymap.set("n", "<leader>gr", ":TSLspRenameFile<CR>", { buffer = true, silent = true })
		vim.keymap.set("n", "<leader>gI", ":TSLspImportAll<CR>", { buffer = true, silent = true })
	end
end

return on_attach
