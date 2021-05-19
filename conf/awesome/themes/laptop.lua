local wibox = require("wibox")

-- inherit default theme
local theme = dofile(os.getenv("HOME") .. "/dotfiles/conf/awesome/themes/common.lua")
theme.font = "FiraCode Nerd Font Normal 10"
theme.wibar_right = function()
	-- Clock Widget:
	local clock_widget = require("widgets.clock")
	-- Setup Weather Widget:
	local volume_widget = require("widgets.volume")
	local battery_widget = require("awesome-wm-widgets.battery-widget.battery")
	local weather_widget = require("awesome-wm-widgets.weather-widget.weather")
	local brightness_widget = require("awesome-wm-widgets.brightness-widget.brightness")
	return {
		layout = wibox.layout.fixed.horizontal,
		spacing = 0,
		brightness_widget == nil and nil or brightness_widget({program="xbacklight", type="icon_and_text"}),
		volume_widget,
		battery_widget == nil and nil or battery_widget({
			show_current_level=true,
		}),
		weather_widget == nil and nil or weather_widget({
			api_key=theme.ow.key,
			coordinates = theme.ow.coordinates,
			units = 'imperial',
			time_format_12h = true,
			both_units_widget = false,
			icons = 'VitalyGorbachev',
			icons_extension = '.svg',
			show_hourly_forecast = true,
			show_daily_forecast = true,
		}),
		wibox.widget.systray(),
		clock_widget,
	}
end
return theme
