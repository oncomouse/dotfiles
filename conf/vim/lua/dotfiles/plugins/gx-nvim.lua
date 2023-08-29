return {
	"chrishrb/gx.nvim",
	keys = {
		{ "<leader>gx", "<Plug>(gx.nvim)", mode = { "n", "v" }, desc = "Open URL using gx.nvim" },
	},
	opts = {},
	init = function()
		local function search_for_url()
			local line = vim.api.nvim_get_current_line()
			local mode = vim.api.nvim_get_mode().mode

			-- cut if in visual mode
			line = require("gx.helper").cut_with_visual_mode(mode, line)

			-- search for url
			local url = require("gx.handler").get_url(
				mode,
				line,
				require("gx").options.handlers,
				require("gx").options.handler_options
			)

			if not url then
				return
			end

			require("gx.shell").execute_with_error(
				require("gx").options.open_browser_app,
				require("gx").options.open_browser_args,
				url
			)
		end
		local opts = { noremap = true, silent = true }
		vim.keymap.set({ "n", "v" }, "<Plug>(gx.nvim)", search_for_url, opts)
	end,
} -- Better URL opening, binds to gx
