local server_maps = require("dotfiles.plugins.nvim-lspconfig.servers")
local on_attach = require("dotfiles.plugins.nvim-lspconfig.on_attach")

local capabilities = nil
local handler_no_diagnostics = {
	["textDocument/publishDiagnostics"] = function() end,
}

for _, servers in pairs(server_maps) do
	vim.api.nvim_create_autocmd("BufReadPre", {
		pattern = servers.pattern,
		group = "dotfiles-settings",
		once = true,
		callback = function()
			if capabilities == nil then
				local ok, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
				if ok then
					capabilities = cmp_nvim_lsp.update_capabilities(vim.lsp.protocol.make_client_capabilities())
				else
					capabilities = vim.lsp.protocol.make_client_capabilities()
				end
				-- Install LSPs
				require("dotfiles.plugins.mason").install_lsp()
			end
			for lsp, settings in pairs(servers.servers) do
				if lsp == "null-ls" then
					vim.cmd([[PackerLoad null-ls.nvim]])
				else
					local opts = {
						on_attach = on_attach,
						capabilities = capabilities,
					}
					if #vim.tbl_keys(settings) > 0 then
						opts = vim.tbl_extend("keep", opts, settings)
					end
					if not vim.tbl_contains(server_maps[lsp].provides or {}, "diagnostics") then
						opts.handlers = handler_no_diagnostics
					end
					require("lspconfig")[lsp].setup(opts)
				end
			end
		end,
	})
end
