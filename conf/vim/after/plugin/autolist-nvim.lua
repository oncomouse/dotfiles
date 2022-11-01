-- Turn off all the bindings in the plugin:
require("autolist").setup({
	colon = {
		preferred = "",
		indent = false,
		indent_raw = false,
	},
	invert = {
		ul_marker = "*",
	},
	normal_mappings = {
		new = { "" },
		tab = { "" },
		detab = { "" },
		recal = { "" },
	},
	insert_mappings = {
		invert = { "" },
		new = { "" },
		detab = { "" },
		tab = { "" },
		recal = { "" },
		indent = { "" },
	},
})
