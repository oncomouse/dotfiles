return {
	{
		"neovim/nvim-lspconfig",
		event = { "BufNewFile", "BufReadPre" },
		dependencies = {
			{ "folke/neodev.nvim", opts = {} },
		},
		init = function()
		end,
		config = function()
			-- Turn on debug-level logging for LSP:
			if vim.g.dotfiles_lsp_debug then
				vim.lsp.set_log_level("trace")
			end
		end,
	},
}
