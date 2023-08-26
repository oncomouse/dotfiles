local __running = {}
local function start_server(server)
	if vim.g.dotfiles_lsp[server] and not __running[server] then
		local config = vim.g.dotfiles_lsp[server] or {}
		config.autostart = false
		__running[server] = true
		local function get_server_capabilities(server)
			local capabilities = vim.lsp.protocol.make_client_capabilities()
			if server.snippets then
				capabilities.textDocument.completion.completionItem.snippetSupport = true
				capabilities.textDocument.completion.completionItem.resolveSupport = {
					properties = {
						"documentation",
						"detail",
						"additionalTextEdits",
					},
				}
			end
			return capabilities
		end

		local opts = {
			capabilities = get_server_capabilities(vim.g.dotfiles_lsp[server]),
		}
		if #vim.tbl_keys(config) > 0 then
			opts = vim.tbl_extend("keep", opts, config)
		end
		require("lspconfig")[server].setup(opts)
		require("lspconfig")[server].launch()
	end
end
return start_server
