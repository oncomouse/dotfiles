return {
	{
		"neovim/nvim-lspconfig",
		lazy = true,
		dependencies = {
			{ "folke/neodev.nvim", lazy = true, opts = {} },
		},
		config = function()
			-- Turn on debug-level logging for LSP:
			if vim.g.dotfiles_lsp_debug then
				vim.lsp.set_log_level("trace")
			end
		end,
	},
}
