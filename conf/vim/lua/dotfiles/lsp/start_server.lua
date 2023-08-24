local __running = {}
local function start_server(server)
	if vim.g.dotfiles_lsp[server] and not __running[server] then
		local config = vim.g.dotfiles_lsp[server]
		config.autostart = false
		__running[server] = true
		require("lspconfig")[server].setup(config)
		require("lspconfig")[server].launch()
	end
end
return start_server
