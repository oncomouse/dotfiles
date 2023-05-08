return {
	"folke/which-key.nvim",
	event = "VeryLazy",
	init = function()
		vim.o.timeout = true
		vim.o.timeoutlen = 150
	end,
	opts = {
		icons = {
			separator = "→",
		},
	},
}
