-- luacheck: globals x
local gears = require("gears")
local beautiful = require("beautiful")

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
return theme
