local beautiful = require("beautiful")
local gears = require("gears")
local awful = require("awful")
-- {{{ Appearance
-- Beautiful
beautiful.init(gears.filesystem.get_themes_dir() .. "xresources/theme.lua")
local xrdb = beautiful.xresources.get_current_theme()
local dpi = beautiful.xresources.apply_dpi
beautiful.bg_focus = xrdb.color6
beautiful.mfact = 0.55
beautiful.tags = { "1", "2", "3", "4", "5", "6", "7", "8", "9" }
beautiful.icon_dir = os.getenv("HOME") .. "/.icons/oomox-xresources-reverse-flat/"
beautiful.font = "FiraCode Nerd Font Normal 10"
beautiful.notification_font = "FiraCode Nerd Font Normal 10"
beautiful.fg_icon = xrdb.color7
beautiful.icon_size = 16
beautiful.notification_icon_size = 16
beautiful.useless_gap = 0 -- No gaps
beautiful.border_color_active = beautiful.bg_focus -- Normal border color
beautiful.border_focus = beautiful.bg_focus -- Focused border color
beautiful.border_width = 1
-- Fonts
beautiful.hotkeys_font = "FiraCode Nerd Font Normal 16"
beautiful.hotkeys_description_font = "FiraCode Nerd Font Normal 12"
-- Wibar stuff:
beautiful.bar_height = 24
beautiful.bar_position = "top"
-- Hotkey formatting:
beautiful.hotkeys_modifiers_fg = xrdb.color4
beautiful.taglist_shape_border_color_focus = xrdb.color5
-- Taglist Formatting:
-- local tag_width = dpi(tonumber(last(gears.string.split(beautiful.font, " "))) + 5)
beautiful.taglist_squares_sel = nil
beautiful.taglist_squares_unsel = nil
-- Titlebar formatting:
beautiful.titlebar_font = "FiraCode Nerd Font Bold 12"
beautiful.titlebar_bg_normal = xrdb.color8
beautiful.titlebar_fg_normal = xrdb.color7
beautiful.titlebar_close_button_focus = gears.filesystem.get_themes_dir() .. "default/titlebar/close_normal.png"
beautiful.titlebar_close_button_focus = gears.filesystem.get_themes_dir() .. "default/titlebar/close_focus.png"
beautiful.titlebar_bg_focus = xrdb.color0
beautiful.titlebar_fg_focus = xrdb.color15
-- Tasklist formatting:
beautiful.tasklist_bg_focus = xrdb.color0
beautiful.tasklist_fg_focus = xrdb.color7
-- Set the background:
beautiful.background_dot_tile_size = dpi(100)
beautiful.background_dot_width = dpi(6)
awful.screen.connect_for_each_screen(function(s)
	require("backgrounds.dots")(s)
end)
awful.util.shell = "/bin/bash"
-- }}}
