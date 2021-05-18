local wibox = require("wibox")

-- inherit default theme
local theme = dofile(os.getenv("HOME") .. "/dotfiles/conf/awesome/themes/common.lua")
theme.name = "desktop"
theme.font = "FiraCode Nerd Font Normal 14"
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
		spacing = 20,
		volume_widget,
		mpris_widget,
		weather_widget == nil and nil or weather_widget({
			api_key=theme.ow.key,
			coordinates = theme.ow.coordinates,
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
