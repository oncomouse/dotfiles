-- luacheck: globals x screen
local gears = require("gears")
local beautiful = require("beautiful")
local wibox = require("wibox")

local theme = dofile(gears.filesystem.get_themes_dir().."xresources/theme.lua")
local xrdb = beautiful.xresources.get_current_theme()

theme.layout_centeredmonocle = gears.color.recolor_image(
	gears.filesystem.get_themes_dir() .. "default/layouts/magnifierw.png",
	theme.fg_normal
)
theme.useless_gap = 0 -- No gaps
theme.border_normal = xrdb.color8 -- Normal border color
theme.border_focus = xrdb.color1 -- Focused border color
theme.border_width = 2
-- Fonts
theme.hotkeys_font = "FiraCode Nerd Font Normal 16"
theme.hotkeys_description_font = "FiraCode Nerd Font Normal 12"
-- Widget spacing in left and right wibox areas:
-- Wibar stuff:
theme.bar_height = 24
theme.bar_position = "top"
-- Hotkey formatting:
theme.hotkeys_modifiers_fg = xrdb.color4
-- Titlebar formatting:
theme.titlebar_font = "FiraCode Nerd Font Bold 12"
theme.titlebar_bg_normal = xrdb.color8
theme.titlebar_fg_normal = xrdb.color7
theme.titlebar_close_button_focus = gears.filesystem.get_themes_dir().."default/titlebar/close_normal.png"
theme.titlebar_close_button_focus  = gears.filesystem.get_themes_dir().."default/titlebar/close_focus.png"
theme.titlebar_bg_focus = xrdb.color0
theme.titlebar_fg_focus = xrdb.color15
-- Tasklist formatting:
theme.tasklist_disable_icon = true -- No icons in tasklist
theme.ow = {
	key=os.getenv("OW_KEY"),
	coordinates={
		tonumber(os.getenv("OW_LAT")),
		tonumber(os.getenv("OW_LONG")),
	}
}
theme.wibar_right = function()
	if screen[1].geometry.width <= 1280 then
		-- Clock Widget:
		local clock_widget = require("widgets.clock")
		-- Setup Weather Widget:
		local volume_widget = require("widgets.volume")
		local battery_widget = require("awesome-wm-widgets.battery-widget.battery")
		local brightness_widget = require("awesome-wm-widgets.brightness-widget.brightness")
		return {
			layout = wibox.layout.fixed.horizontal,
			spacing = 0,
			brightness_widget == nil and nil or brightness_widget({program="xbacklight", type="icon_and_text"}),
			volume_widget,
			battery_widget == nil and nil or battery_widget({
				show_current_level=true,
			}),
			wibox.widget.systray(),
			clock_widget,
		}
	end
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
		wibox.widget.systray(),
		clock_widget,
	}
end
return theme
