return {
	{
		"rcarriga/nvim-notify",
		opts = {
			timeout = 3000,
		},
		event = "VeryLazy",
		keys = {
			{ mode = "n", "<leader>nc", "<cmd>lua require('notify').dismiss()<cr>", desc = "Clear all notifications" },
			{ mode = "n", "<leader>nn", "<cmd>Notifications<cr>", desc = "Display notification history" },
		},
		config = function(_, opts)
			require("notify").setup(opts)
			vim.notify = require("notify")
		end,
	}
}
