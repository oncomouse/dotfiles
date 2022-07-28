local function config_lspconfig()
	local servers = require("dotfiles.plugins.nvim-lspconfig.servers")
	local on_attach = require("dotfiles.plugins.nvim-lspconfig.on_attach")

	-- Install LSPs
	require("dotfiles.plugins.mason").install_lsp()

	vim.diagnostic.config({
		underline = true,
		virtual_text = true,
		signs = false,
		severity_sort = true,
	})

	-- LSP Logging:
	-- vim.lsp.set_log_level("trace")

	local handler_no_diagnostics = {
		["textDocument/publishDiagnostics"] = function() end,
	}
	local capabilities = vim.lsp.protocol.make_client_capabilities()

	for lsp, settings in pairs(servers) do
		local opts = {
			on_attach = on_attach,
			capabilities = capabilities,
		}
		if #vim.tbl_keys(settings) > 0 then
			opts = vim.tbl_extend("keep", opts, settings)
		end
		if not vim.tbl_contains(servers[lsp].provides or {}, "diagnostics") then
			opts.handlers = handler_no_diagnostics
		end
		if lsp ~= "null-ls" then
			require("lspconfig")[lsp].setup(opts)
		end
	end
end

return config_lspconfig
