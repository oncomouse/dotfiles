local wezterm = require("wezterm")

local function nerdify_font(font, params)
	font = type(font) == "table" and font or {
		family = font,
	}
	return wezterm.font_with_fallback({
		font,
		{
			family = "FiraCode Nerd Font",
		},
	}, params)
end

return nerdify_font
