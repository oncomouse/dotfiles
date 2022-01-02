-- luacheck: globals vim dotfiles
local map = require("dotfiles.utils.map")
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
	map.nnoremap("<silent><buffer>", "<leader>s", function()
		vim.lsp.buf.document_symbol()
	end)
	map.nnoremap("<silent><buffer>", "<F2>", function()
		vim.lsp.buf.rename()
	end)
	map.nnoremap("<silent><buffer>", "gd", function()
		vim.lsp.buf.definition()
	end)
	if client.resolved_capabilities.goto_definition == true then
		vim.opt_local.tagfunc = "v:lua.vim.lsp.tagfunc"
	end
	map.nnoremap("<silent><buffer>", "gD", function()
		vim.lsp.buf.declaration()
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
		require("dotfiles.nvim_lsp.show_documentation")()
	end)
	map.nnoremap("<silent><buffer>", "<C-k>", function()
		vim.lsp.buf.signature_help()
	end)
	map.nnoremap("<silent><buffer>", "<F5>", ":<CR>")
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
		map.nnoremap("<silent><buffer>", "]d", function()
			vim.diagnostic.goto_next()
		end)
		map.nnoremap("<silent><buffer>", "[d", function()
			vim.diagnostic.goto_prev()
		end)
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
		map.nmap("<buffer><silent>", "gs", ":TSLspOrganize<CR>")
		map.nmap("<buffer><silent>", "gr", ":TSLspRenameFile<CR>")
		map.nmap("<buffer><silent>", "gI", ":TSLspImportAll<CR>")
	end
end

return on_attach
