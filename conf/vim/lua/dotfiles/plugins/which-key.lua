return {
	"folke/which-key.nvim",
	init = function()
		vim.o.timeout = true
		vim.o.timeoutlen = 250
	end,
	opts = {
		icons = {
			separator = "→",
		},
		window = {
			margin = { 1, 0, 0, 0 },
		}
	},
}
