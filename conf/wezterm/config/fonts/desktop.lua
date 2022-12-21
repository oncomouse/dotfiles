local nerdify_font = require("utils.nerdify_font")

return {
	font = nerdify_font("Fira Code"),
	font_rules = {
		{
			italic = true,
			font = nerdify_font("Hasklig", { italic = true }),
		},
	},
	font_size = 18.0,
	-- harfbuzz_features = { "zero", "ss02" },
}
