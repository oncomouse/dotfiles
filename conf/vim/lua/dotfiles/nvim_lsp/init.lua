vim.diagnostic.config({
	underline = true,
	virtual_text = true,
	signs = false,
	severity_sort = true,
})
local servers = require("dotfiles.nvim_lsp.servers")
local lsp_installer_servers = require("nvim-lsp-installer.servers")

-- LSP Logging:
-- vim.lsp.set_log_level("trace")

local handler_no_diagnostics = {
	["textDocument/publishDiagnostics"] = function() end,
}

local on_attach = require("dotfiles.nvim_lsp.on_attach")

local capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities())

require("dotfiles.null-ls")

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
	local ok, lsp_server = lsp_installer_servers.get_server(lsp)
	if ok then
		lsp_server:on_ready(function()
			lsp_server:setup(opts)
		end)
		if not lsp_server:is_installed() then
			lsp_server:install()
		end
	elseif vim.tbl_contains(require("lspconfig").available_servers(), lsp) then
		require("lspconfig")[lsp].setup(opts)
	end
end
