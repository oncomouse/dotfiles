if pcall(require, "lspconfig") then
	local servers = require("dotfiles.plugins.nvim-lspconfig")

	local capabilities = nil
	local handler_no_diagnostics = {
		["textDocument/publishDiagnostics"] = function() end,
	}

	-- Turn on debug-level logging for LSP:
	if vim.g.dotfiles_lsp_debug then
		vim.lsp.set_log_level("trace")
	end

	vim.api.nvim_create_autocmd("BufReadPre", {
		pattern = servers.pattern,
		group = "dotfiles-settings",
		once = true,
		callback = function()
			if capabilities == nil then
				local ok, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
				if ok then
					capabilities = cmp_nvim_lsp.default_capabilities()
				else
					capabilities = vim.lsp.protocol.make_client_capabilities()
				end
			end
			for _, lsp in pairs(servers.servers) do
				local settings = servers[lsp]
				if lsp ~= "null-ls" then
					local opts = {
						on_attach = servers.on_attach,
						capabilities = capabilities,
					}
					if #vim.tbl_keys(settings) > 0 then
						opts = vim.tbl_extend("keep", opts, settings)
					end
					if not vim.tbl_contains(servers[lsp].provides or {}, "diagnostics") then
						opts.handlers = handler_no_diagnostics
					end
					require("lspconfig")[lsp].setup(opts)
				end
			end
		end,
	})
end
