return {
	"folke/zen-mode.nvim",
	cmd = "ZenMode",
	keys = {
		{ "<leader>gz", "<cmd>ZenMode<cr>", desc = "ZenMode" }
	},
	opts = {
		window = {
			backdrop = 1,
		},
		plugins = {
			tmux = {
				enabled = true,
			},
		},
	},
}
