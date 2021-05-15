-- luacheck: globals awesome
local wibox = require("wibox")
local beautiful = require("beautiful")
local gfs = require("gears.filesystem")
local themes_path = gfs.get_themes_dir()
-- Volume Widget:
-- Clock Widget:
local clock_widget = require("widgets.clock")
-- Setup Weather Widget:
local volume_widget = nil
local weather_widget = nil
local battery_widget = nil
if not gfs.file_readable(os.getenv("HOME").."/.config/awesome/json.lua") then
	volume_widget = require('awesome-wm-widgets.volume-widget.volume')
	battery_widget = require("awesome-wm-widgets.battery-widget.battery")
	weather_widget = require("awesome-wm-widgets.weather-widget.weather")
end

-- inherit default theme
local theme = dofile(themes_path.."xresources/theme.lua")
theme.font = "FiraCode Nerd Font Normal 12"
theme.wibar_right = {
			layout = wibox.layout.fixed.horizontal,
			spacing_widget = {
				text = beautiful.widget_space.right,
				widget = wibox.widget.textbox,
			},
			spacing = 20,
			volume_widget == nil and nil or volume_widget(),
			battery_widget == nil and nil or battery_widget(),
			weather_widget == nil and nil or weather_widget({
				api_key='7092b2d31fe650e586336bc51e657814',
				coordinates = {30.663606864996588, -96.34546254147835},
				units = 'imperial',
				time_format_12h = true,
				both_units_widget = false,
				icons = 'VitalyGorbachev',
				icons_extension = '.svg',
				show_hourly_forecast = true,
				show_daily_forecast = true,
			}),
			clock_widget,
		}
return theme
