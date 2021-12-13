-- luacheck: globals vim dotfiles
vim.diagnostic.config({
	underline = true,
	virtual_text = true,
	signs = false,
	severity_sort = true,
})
local servers = {
	["null-ls"] = {
		provides = {
			"diagnostics",
			"formatting",
		},
	},
	sumneko_lua = {
		provides = {
			"snippets",
		},
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
		flags = {
			debounce_text_changes = 500,
		},
	},
	cssls = {
		provides = {
			"snippets",
			"diagnostics",
		},
		flags = {
			debounce_text_changes = 500,
		},
	},
	html = {
		provides = {
			"snippets",
		},
		flags = {
			debounce_text_changes = 500,
		},
	},
	jsonls = {
		filetypes = { "json", "jsonc" },
		provides = {
			"snippets",
			"diagnostics",
		},
		flags = {
			debounce_text_changes = 500,
		},
	},
	solargraph = {},
	vimls = {
		flags = {
			debounce_text_changes = 500,
		},
		init_options = {
			isNeovim = true,
			diagnostic = {
				enable = false,
			},
		},
		provides = {
			"snippets",
		},
	},
	bashls = {
		flags = {
			debounce_text_changes = 500,
		},
	},
	pyright = {
		flags = {
			debounce_text_changes = 500,
		},
	},
	tsserver = {
		provides = {
			"diagnostics",
		},
		flags = {
			debounce_text_changes = 500,
		},
	},
}

local lsp_installer_servers = require("nvim-lsp-installer.servers")
local map = require("dotfiles.utils.map")

-- LSP Logging:
-- vim.lsp.set_log_level("trace")

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

local on_attach = function(client, _)
	-- Update codeLens:
	if client.resolved_capabilities.code_lens then
		vim.api.nvim_command([[autocmd CursorHold,CursorHoldI,InsertLeave <buffer> lua vim.lsp.codelens.refresh()]])
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

require("null-ls").setup({
	on_attach = on_attach,
	sources = {
		require("null-ls").builtins.formatting.prettier.with({
			update_on_insert = false,
			extra_args = { "--use-tabs" },
			filetypes = {
				"vue",
				"svelte",
				"css",
				"scss",
				"less",
				"html",
				"json",
				"markdown",
				"graphql",
			},
			prefer_local = "node_modules/.bin",
		}),
		require("null-ls").builtins.formatting.prettier.with({
			filetypes = {
				"yaml",
			},
			prefer_local = "node_modules/.bin",
		}),
		require("null-ls").builtins.formatting.stylua,
		require("null-ls").builtins.formatting.black.with({
			extra_args = { "-l", "79" }, -- PEP8 line lengths
		}),
		require("null-ls").builtins.formatting.reorder_python_imports,
		require("null-ls").builtins.formatting.fish_indent,
		require("null-ls").builtins.formatting.shfmt,
		require("null-ls").builtins.formatting.rubocop,
		require("null-ls").builtins.formatting.standardrb,
		require("dotfiles.null-ls.builtins.formatting.semistandard"),
		require("null-ls").builtins.diagnostics.shellcheck,
		require("null-ls").builtins.diagnostics.luacheck,
		require("null-ls").builtins.diagnostics.flake8,
		require("null-ls").builtins.diagnostics.vint,
		require("null-ls").builtins.diagnostics.rubocop,
		require("null-ls").builtins.diagnostics.standardrb,
		require("dotfiles.null-ls.builtins.diagnostics.semistandard"),
		require("dotfiles.null-ls.builtins.diagnostics.yamllint"),
		require("dotfiles.null-ls.builtins.completion.bibtex"),
		require("dotfiles.null-ls.builtins.hover.bibtex"),
		-- require("null-ls").builtins.completion.vsnip,
		-- require("dotfiles.null-ls.builtins.hover.dictionary"),
	},
})

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

for lsp, settings in pairs(servers) do
	local ok, lsp_server = lsp_installer_servers.get_server(lsp)
	if ok then
		local opts = {
			on_attach = on_attach,
			capabilities = capabilities,
		}
		if #vim.tbl_keys(settings) > 0 then
			opts = vim.tbl_extend("keep", opts, settings)
		end
		-- local snippet_provider = vim.tbl_contains(servers[lsp].provides or {}, "snippets")
		-- if snippet_provider then
		-- 	opts.capabilities = vscode_capabilities
		-- end
		local diagnostic_provider = vim.tbl_contains(servers[lsp].provides or {}, "diagnostics")
		if not diagnostic_provider then
			opts.handlers = handler_no_diagnostics
		end
		lsp_server:on_ready(function()
			lsp_server:setup(opts)
		end)
		if not lsp_server:is_installed() then
			lsp_server:install()
		end
	end
end
