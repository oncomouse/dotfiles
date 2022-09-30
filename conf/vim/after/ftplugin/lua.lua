local spec_pair = require("mini.ai").gen_spec.pair

 -- s for [[]] in ai and surround
vim.b.miniai_config = {
	custom_textobjects = {
		["s"] = spec_pair("[[", "]]"),
	},
}
vim.b.minisurround_config = {
	custom_surroundings = {
		s = {
			input = { '%[%[().-()%]%]' },
			output = { left = "[[", right = "]]" },
		},
	},
}
