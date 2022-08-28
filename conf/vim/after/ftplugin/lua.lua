vim.b.minisurround_config = {
	custom_surroundings = {
		s = { -- s for [[]]
			input = { '%[%[().-()%]%]' },
			output = { left = "[[", right = "]]" },
		},
	},
}
