-- luacheck: globals awesome
local wibox = require("wibox")
local gfs = require("gears").filesystem

-- inherit default theme
local theme = dofile(gfs.get_themes_dir().."xresources/theme.lua")
theme.name = "desktop"
theme.font = "FiraCode Nerd Font Normal 14"
theme.widget_space = {
	left = nil,
	right = "⁞",
}
theme.wibar_right = function()
	-- Mpris Player Status Widget:
	local mpris_widget = require("widgets.mpris")
	-- Volume Widget:
	local volume_widget = require("widgets.volume")
	-- Clock Widget:
	local clock_widget = require("widgets.clock")
	-- Setup Weather Widget:
	local weather_widget = require("awesome-wm-widgets.weather-widget.weather")

	return {
		layout = wibox.layout.fixed.horizontal,
		spacing_widget = {
			text = theme.widget_space.right,
			widget = wibox.widget.textbox,
		},
		spacing = 20,
		volume_widget,
		mpris_widget,
		weather_widget == nil and nil or weather_widget({
			api_key='7092b2d31fe650e586336bc51e657814',
			coordinates = {30.663606864996588, -96.34546254147835},
			units = 'imperial',
			time_format_12h = true,
			both_units_widget = false,
			-- font_name = 'Carter One',
			icons = 'VitalyGorbachev',
			icons_extension = '.svg',
			show_hourly_forecast = true,
			show_daily_forecast = true,
		}),
		clock_widget,
	}
end
return theme
