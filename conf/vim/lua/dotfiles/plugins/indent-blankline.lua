return {
	"lukas-reineke/indent-blankline.nvim",
	opts = {
		indent = { char = "▏" },
		scope = { enabled = false },
		exclude = {
			filetypes = {
				"help",
				"alpha",
				"dashboard",
				"neo-tree",
				"Trouble",
				"lazy",
				"mason",
				"notify",
				"toggleterm",
				"lazyterm",
			},
		},
	},
	main = "ibl", -- This doesn't work
}
