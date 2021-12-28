local XDG_CACHE_HOME = require("config.env").XDG_CACHE_HOME

return {
	-- Color scheme:
	color_scheme_dirs = {
		XDG_CACHE_HOME .. "/wal",
	},
	color_scheme = "wal",
	hide_tab_bar_if_only_one_tab = true,
}
