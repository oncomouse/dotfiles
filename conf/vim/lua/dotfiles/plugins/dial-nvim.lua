return {
	"monaqa/dial.nvim",
	keys = {
		{ "<C-a>", "<Plug>(dial-increment)", mode = "n" },
		{ "<C-x>", "<Plug>(dial-decrement)", mode = "n" },
		{ "<C-a>", "<Plug>(dial-increment)", mode = "v" },
		{ "<C-x>", "<Plug>(dial-decrement)", mode = "v" },
		{ "g<C-a>", "g<Plug>(dial-increment)", mode = "v" },
		{ "g<C-x>", "g<Plug>(dial-decrement)", mode = "v" },
	},
	config = function()
		local augend = require("dial.augend")
		require("dial.config").augends:register_group({
			-- default augends used when no group name is specified
			default = {
				augend.integer.alias.decimal, -- nonnegative decimal number (0, 1, 2, 3, ...)
				augend.integer.alias.hex, -- nonnegative hex number  (0x01, 0x1a1f, etc.)
				augend.constant.alias.bool, -- boolean value (true <-> false)
				augend.date.alias["%Y/%m/%d"], -- date (2022/02/18, etc.)
				augend.date.alias["%m/%d/%Y"], -- date (02/19/2022)
				augend.date.alias["%m-%d-%Y"], -- date (02-19-2022)
				augend.date.alias["%Y-%m-%d"], -- date (02-19-2022)
				augend.date.new({
					pattern = "%m.%d.%Y",
					default_kind = "day",
					only_valid = true,
					word = false,
				}),
			},
		})
	end,
}
