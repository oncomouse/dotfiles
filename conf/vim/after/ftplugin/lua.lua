vim.b.minisurround_config = {
	custom_surroundings = {
		s = { -- s for [[]]
			input = { find = "%[%[.-%]%]", extract = "^(..).*(..)$" },
			output = { left = "[[", right = "]]" },
		},
	},
}
