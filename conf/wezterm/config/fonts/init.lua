-- local DOTFILES_TARGET = require("config.env").DOTFILES_TARGET

-- local function get_fonts()
-- 	local ok, config = pcall(require, "config.fonts." .. DOTFILES_TARGET)
-- 	if ok then
-- 		return config
-- 	end
-- 	return {}
-- end

-- return get_fonts()
local nerdify_font = require("utils.nerdify_font")
return {
	font = nerdify_font("Fira Code"),
	font_rules = {
		{
			italic = true,
			font = nerdify_font("Hasklig", { italic = true }),
		},
	},
	font_size = 14.0,
	-- harfbuzz_features = { "zero", "ss02" },
}
