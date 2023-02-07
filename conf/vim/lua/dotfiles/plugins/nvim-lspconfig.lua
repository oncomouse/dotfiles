return {
	"neovim/nvim-lspconfig",
	event = "BufReadPre",
	config = function()
		local servers = require("dotfiles.nvim-lspconfig")

		local capabilities = nil
		local snippet_capabilities = vim.lsp.protocol.make_client_capabilities()
		snippet_capabilities.textDocument.completion.completionItem.snippetSupport = true
		snippet_capabilities.textDocument.completion.completionItem.resolveSupport = {
			properties = {
				"documentation",
				"detail",
				"additionalTextEdits",
			},
		}
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
				local has_cmp, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
				local function get_server_capabilities(server)
					if capabilities == nil then
						if has_cmp then
							capabilities = cmp_nvim_lsp.default_capabilities()
						else
							capabilities = vim.lsp.protocol.make_client_capabilities()
						end
					end
					if has_cmp then
						return capabilities
					end
					if vim.tbl_contains(server.provides or {}, "snippets") then
						return snippet_capabilities
					end
					return capabilities
				end
				for _, lsp in pairs(servers.servers) do
					local settings = servers[lsp]
					if lsp ~= "null-ls" then
						local opts = {
							on_attach = servers.on_attach,
							capabilities = get_server_capabilities(servers[lsp]),
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
	end,
}
