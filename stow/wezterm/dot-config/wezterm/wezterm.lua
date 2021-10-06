local wezterm = require("wezterm")

return {
	font = wezterm.font_with_fallback({
		"Fira Code",
		"FiraCode Nerd Font",
	}),
	font_antialias = "Subpixel",
	font_rules = {
		{
			italic = true,
			font = wezterm.font("Fira Code", { italic = false }),
		},
		{
			italic = true,
			intensity = "Bold",
			font = wezterm.font("Fira Code", { bold = true, italic = false }),
		},
	},
	font_size = os.getenv("DOTFILES_TARGET") == "laptop" and 11.0 or 18.0,
	hide_tab_bar_if_only_one_tab = true,
	color_scheme_dirs = { wezterm.home_dir .. "/.cache/wal/" },
	color_scheme = "wal",
}
