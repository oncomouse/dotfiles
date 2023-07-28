return {
	"rcarriga/nvim-notify",
	opts = {
		timeout = 3000,
	},
	keys = {
		{ mode = "n", "<leader>nc", "lua require('nvim-notify').clear()", desc = "Clear all notifications" },
	},
	config = function(_, opts)
		require("notify").setup(opts)
		vim.notify = require("notify")
	end,
}
