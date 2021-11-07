-- luacheck: globals vim dotfiles
local lspconfig = require("lspconfig")
local lsp_installer_servers = require("nvim-lsp-installer.servers")
local map = require("dotfiles.utils.map")

-- LSPs that provide diagnostics:
local diagnostics_providers = {
	"null-ls",
	"cssls",
	"jsonls",
	"tsserver",
	"rust_analyzer",
}

-- LSPs that provide snippets
local snippet_providers = {
	"html",
	"cssls",
	"jsonls",
}

-- LSPs that provide formatting:
local formatting_providers = {
	"null-ls",
	"rust_analyzer",
}

local handler_no_diagnostics = {
	["textDocument/publishDiagnostics"] = function() end,
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

local on_attach = function(client, _)
	-- Update codeLens:
	if client.resolved_capabilities.code_lens then
		vim.api.nvim_command([[autocmd CursorHold,CursorHoldI,InsertLeave <buffer> lua vim.lsp.codelens.refresh()]])
	end
	-- Use C+x C+o for completion:
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
		show_documentation()
	end)
	map.nnoremap("<silent><buffer>", "<C-k>", function()
		vim.lsp.buf.signature_help()
	end)
	map.nnoremap("<silent><buffer>", "<F5>", ":<CR>")
	if vim.tbl_contains(snippet_providers, client.name) then
		vim.cmd([[packadd packer.nvim | lua require("packer").loader("vim-vsnip-integ")]])
	end
	if vim.tbl_contains(diagnostics_providers, client.name) then
		if vim.diagnostic ~= nil then -- Neovim 0.6:
			vim.cmd([[
				autocmd! dotfiles-settings User DiagnosticsChanged lua vim.diagnostic.setloclist({ open = false })
			]])
			map.nnoremap("<silent><buffer>", "]d", function()
				vim.diagnostic.goto_next()
			end)
			map.nnoremap("<silent><buffer>", "[d", function()
				vim.diagnostic.goto_prev()
			end)
		else -- Neovim 0.5:
			vim.cmd([[
				autocmd! dotfiles-settings User LspDiagnosticsChanged lua vim.lsp.diagnostic.set_loclist({ open_loclist = false })
			]])
			map.nnoremap("<silent><buffer>", "]d", function()
				vim.lsp.diagnostic.goto_next()
			end)
			map.nnoremap("<silent><buffer>", "[d", function()
				vim.lsp.diagnostic.goto_prev()
			end)
		end
	end
	-- Formatting:
	if vim.tbl_contains(formatting_providers, client.name) then
		vim.cmd([[command! -buffer Format lua vim.lsp.buf.formatting()]])
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
			enable_import_on_completion = false,

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

			-- eslint
			eslint_enable_code_actions = true,
			eslint_enable_disable_comments = true,
			eslint_bin = "eslint",
			eslint_enable_diagnostics = false,
			eslint_opts = {},

			-- formatting
			enable_formatting = false,
			formatter = "prettier",
			formatter_opts = {},

			-- update imports on file move
			update_imports_on_move = false,
			require_confirmation_on_move = false,
			watch_dir = nil,

			-- filter diagnostics
			filter_out_diagnostics_by_severity = {},
			filter_out_diagnostics_by_code = {},
		})

		-- required to fix code action ranges and filter diagnostics
		ts_utils.setup_client(client)

		-- no default maps, so you may want to define some here
		map.nmap("<buffer><silent>", "gs", ":TSLspOrganize<CR>")
		map.nmap("<buffer><silent>", "gr", ":TSLspRenameFile<CR>")
		map.nmap("<buffer><silent>", "gI", ":TSLspImportAll<CR>")
	end
end

local servers = {
	sumneko_lua = {
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
	cssls = {},
	html = {},
	jsonls = {
		filetypes = { "json", "jsonc" },
	},
	solargraph = {},
	vimls = {},
	bashls = {},
	pyright = {},
	tsserver = {},
	["rust_analyzer"] = {},
}
lspconfig["null-ls"].setup({
	on_attach = on_attach,
})
for lsp, settings in pairs(servers) do
	local opts = {
		on_attach = on_attach,
	}
	if #vim.tbl_keys(settings) > 0 then
		opts = vim.tbl_extend("keep", opts, settings)
	end
	if vim.tbl_contains(snippet_providers, lsp) then
		opts.capabilities = vscode_capabilities
	end
	if not vim.tbl_contains(diagnostics_providers, lsp) then
		opts.handlers = handler_no_diagnostics
	end
	local ok, lsp_server = lsp_installer_servers.get_server(lsp)
	if ok then
		lsp_server:on_ready(function()
			lsp_server:setup(opts)
		end)
		if not lsp_server:is_installed() then
			lsp_server:install()
		end
	end
end
