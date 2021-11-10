local lsp_types = {
	"css",
	"fish",
	"html",
	"javascript",
	"javascriptreact",
	"json",
	"jsonc",
	"lua",
	"markdown",
	"python",
	"ruby",
	"rust",
	"scss",
	"sh",
	"typescript",
	"typescriptreact",
	"vim",
	"yaml",
}
return {
	"neovim/nvim-lspconfig",
	requires = {
		{ "williamboman/nvim-lsp-installer", module = "nvim-lsp-installer" },
		{ "hrsh7th/vim-vsnip-integ", opt = true, requires = { "vim-vsnip" } },
		{
			"jose-elias-alvarez/null-ls.nvim",
			module = "null-ls",
			requires = { { "nvim-lua/plenary.nvim", module = "plenary" } },
		},
		{
			"jose-elias-alvarez/nvim-lsp-ts-utils",
			ft = { "javascript", "javascriptreact", "typescript", "typescriptreact" },
		},
	},
	ft = lsp_types,
	config = function()
		require("dotfiles.nvim_lsp")
	end,
}
