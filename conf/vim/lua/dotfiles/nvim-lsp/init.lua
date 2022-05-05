vim.diagnostic.config({
	underline = true,
	virtual_text = true,
	signs = false,
	severity_sort = true,
})
local servers = require("dotfiles.nvim-lsp.servers")

-- LSP Logging:
-- vim.lsp.set_log_level("trace")

local handler_no_diagnostics = {
	["textDocument/publishDiagnostics"] = function() end,
}

local on_attach = require("dotfiles.nvim-lsp.on_attach")

-- local capabilities = require("cmp_nvim-lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities())
local capabilities = vim.lsp.protocol.make_client_capabilities()

require("dotfiles.null-ls")
require("nvim-lsp-installer").setup({
	ensure_installed = vim.tbl_keys(servers),
})

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
