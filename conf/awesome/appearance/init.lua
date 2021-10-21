-- luacheck: globals screen
local beautiful = require("beautiful")
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
local is_laptop = require("utils.is_laptop")
-- {{{ Appearance
-- Beautiful
beautiful.init(gears.filesystem.get_themes_dir() .. "xresources/theme.lua")
local xrdb = beautiful.xresources.get_current_theme()
local dpi = beautiful.xresources.apply_dpi
beautiful.bg_focus = xrdb.color6
beautiful.mfact = 0.55
beautiful.tags = { "1", "2", "3", "4", "5", "6", "7", "8", "9" }
beautiful.icon_dir = os.getenv("HOME") .. "/.icons/oomox-xresources-reverse-flat/"
beautiful.fg_icon = xrdb.color7
beautiful.icon_size = 16
beautiful.notification_icon_size = 16
beautiful.useless_gap = 0 -- No gaps
beautiful.cm_scale = is_laptop and 0.85 or 0.75
-- Border Colors:
beautiful.border_color_normal = xrdb.color0
beautiful.border_color_active = beautiful.bg_focus
beautiful.border_color_floating_normal = xrdb.color0
beautiful.border_color_floating_active = beautiful.border_color_active
beautiful.border_width = 1
-- Fonts:
beautiful.font = is_laptop and "Hack Nerd Font Normal 9" or "FiraCode Nerd Font Normal 10"
beautiful.notification_font = is_laptop and "Hack Nerd Font Normal 9" or "FiraCode Nerd Font Normal 10"
beautiful.hotkeys_font = is_laptop and "Hack Nerd Font Normal 12" or "FiraCode Nerd Font Normal 14"
beautiful.hotkeys_description_font = is_laptop and "Hack Nerd Font Normal 12" or "FiraCode Nerd Font Normal 14"
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
-- Load Additional Resources:
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
local naughty = require("naughty")
naughty.connect_signal("request::display_error", function(message, startup)
	naughty.notification({
		urgency = "critical",
		title = "Oops, an error happened" .. (startup and " during startup!" or "!"),
		message = message,
	})
end)
naughty.connect_signal('request::display', function(n)
   naughty.layout.box{notification = n}
end)
require("layouts") -- Layouts
-- Screen decoration settings
screen.connect_signal("request::desktop_decoration", function(s)
	-- Attach tags:
	for _, t in ipairs(beautiful.tags) do
		awful.tag.add(t, {
			screen = s,
			layout = awful.layout.layouts[1],
			master_width_factor = beautiful.mfact,
		})
	end
	-- Highlight first tag to start:
	awful.screen.focused().tags[1]:view_only()
end)
require("appearance.behaviors.sloppyfocus")
-- require("appearance.behaviors.attachbelow") -- attachbelow patch
require("appearance.behaviors.smartborders") -- smartborder patch
