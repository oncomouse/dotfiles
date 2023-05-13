return {
	{
		"neovim/nvim-lspconfig",
		event = { "BufNewFile", "BufReadPre" },
		dependencies = {
			{ "folke/neodev.nvim", opts = {} },
		},
		config = function()
			local servers = require("dotfiles.plugins.lsp.servers")

			-- Turn on debug-level logging for LSP:
			if vim.g.dotfiles_lsp_debug then
				vim.lsp.set_log_level("trace")
			end

			-- Add diagnostics to loclist:
			vim.api.nvim_create_autocmd({ "DiagnosticChanged" }, {
				group = vim.api.nvim_create_augroup("dotfiles-lsp-attach_diagnostics", {}),
				pattern = "*",
				callback = function()
					vim.diagnostic.setloclist({ open = false })
				end,
			})

			-- Only attach each server once:
			for lsp, settings in pairs(servers) do
				local pattern = settings.pattern
				if type(pattern) == "table" then
					pattern = table.concat(vim.fn.uniq(vim.fn.sort(pattern)), ",")
				end
				vim.api.nvim_create_autocmd({ "BufNewFile", "BufReadPre" }, {
					pattern = pattern,
					group = "dotfiles-settings",
					once = true,
					callback = function()
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
							capabilities.textDocument.foldingRange = {
								dynamicRegistration = false,
								lineFoldingOnly = true,
							}
							return capabilities
						end

						local opts = {
							on_attach = require("dotfiles.plugins.lsp.on_attach"),
							capabilities = get_server_capabilities(servers[lsp]),
						}
						if #vim.tbl_keys(settings) > 0 then
							opts = vim.tbl_extend("keep", opts, settings)
						end
						require("lspconfig")[lsp].setup(opts)
					end,
				})
			end
		end,
	},
}
