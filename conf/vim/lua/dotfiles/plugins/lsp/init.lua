return {
	{
		"neovim/nvim-lspconfig",
		event = { "BufNewFile", "BufReadPre" },
		dependencies = {
			{ "folke/neodev.nvim", opts = {} },
		},
		init = function()
			-- Set to true for debug logging in LSP:
			vim.g.dotfiles_lsp_debug = false

			-- Use LspAttach event:
			vim.api.nvim_create_autocmd("LspAttach", {
				group = vim.api.nvim_create_augroup("dotfiles-lsp-on-attach", {}),
				callback = function(ev)
					require("dotfiles.plugins.lsp.on_attach")(vim.lsp.get_client_by_id(ev.data.client_id), ev.buf)
				end,
			})
		end,
		config = function()
			local servers = require("dotfiles.plugins.lsp.servers")

			-- Turn on debug-level logging for LSP:
			if vim.g.dotfiles_lsp_debug then
				vim.lsp.set_log_level("trace")
			end

			-- Only attach each server once:
			for lsp, settings in pairs(servers) do
				local pattern = settings.pattern
				if type(pattern) == "table" then
					pattern = table.concat(vim.fn.uniq(vim.fn.sort(pattern)), ",")
				end
				vim.api.nvim_create_autocmd({ "BufNewFile", "BufReadPre" }, {
					pattern = pattern,
					group = vim.api.nvim_create_augroup("dotfiles-lsp-config-lsp", {}),
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
							return capabilities
						end

						local opts = {
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
