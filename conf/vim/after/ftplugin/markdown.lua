local spec_pair = require("mini.ai").gen_spec.pair

vim.b.miniai_config = {
	custom_textobjects = {
		["*"] = spec_pair("*", "*", { type = "greedy" }),
		["_"] = spec_pair("_", "_", { type = "greedy" }),
	},
}
