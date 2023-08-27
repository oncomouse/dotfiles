local function get_server_capabilities(server_config)
	local capabilities = vim.lsp.protocol.make_client_capabilities()
	if server_config.snippets then
		capabilities.textDocument.completion.completionItem.snippetSupport = true
		capabilities.textDocument.completion.completionItem.resolveSupport = {
			properties = {
				"documentation",
				"detail",
				"additionalTextEdits",
			},
		}
	end
	-- See: https://github.com/neovim/neovim/issues/23291
	capabilities.workspace.didChangeWatchedFiles.dynamicRegistration = false
	return capabilities
end

local function start_server(server)
	local running = vim.lsp.get_clients({ name = server })
	if #running == 0 then
		local config = vim.g.dotfiles_lsp[server] or {}
		config.autostart = false

		local opts = vim.tbl_extend("keep", {
			capabilities = get_server_capabilities(vim.g.dotfiles_lsp[server]),
		}, config)

		require("lspconfig")[server].setup(opts)
		require("lspconfig")[server].launch()
	else
		vim.lsp.buf_attach_client(0, running[1].id)
	end
end
return start_server
