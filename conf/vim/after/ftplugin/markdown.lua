local spec_pair = require("mini.ai").gen_spec.pair

vim.b.miniai_config = {
	custom_textobjects = {
		["*"] = spec_pair("*", "*", { type = "greedy" }), -- Grab all asterisks when selecting
		["_"] = spec_pair("_", "_", { type = "greedy" }), -- Grab all underscores when selecting
	},
}

vim.b.minisurround_config = {
	custom_surroundings = {
		["b"] = { -- Surround for bold
			input = { "%*%*().-()%*%*" },
			output = { left = "**", right = "**" },
		},
		["i"] = { -- Surround for italics
			input = { "%*().-()%*" },
			output = { left = "*", right = "*" },
		},
	},
}
