local beautiful = require("beautiful")
local awful = require("awful")
local is_laptop = require("utils.is_laptop")
local menubar = require("menubar")
local xrdb = beautiful.xresources.get_current_theme()
local dpi = beautiful.xresources.apply_dpi
-- {{{ Appearance
-- Load Theme
beautiful.floating_only = is_laptop -- Are we only floating?
beautiful.init("~/dotfiles/conf/awesome/themes/xresources/theme.lua")
-- Centered zoom scale:
beautiful.cm_scale = is_laptop and 0.85 or 0.75
-- Main area ratio:
beautiful.mfact = 0.55
-- Fonts {{{
local laptop_font = "JetBrainsMono Nerd Font"
local desktop_font = "FiraCode Nerd Font"
beautiful.font = is_laptop and laptop_font .. " Normal 9" or desktop_font .. " Normal 10"
beautiful.notification_font = is_laptop and laptop_font .. " Normal 9" or desktop_font .. " Normal 10"
beautiful.hotkeys_font = is_laptop and laptop_font .. " Normal 12" or desktop_font .. " Normal 14"
beautiful.hotkeys_description_font = is_laptop and laptop_font .. " Normal 12" or desktop_font .. " Normal 14"
beautiful.titlebar_font = is_laptop and laptop_font .. " Normal 9" or desktop_font .. " Normal 10"
beautiful.titlebar_title_enabled = true
-- }}}
beautiful.icon_theme = "/usr/share/icons/ePapirus-Dark"
-- Tags:
beautiful.tags = { "1", "2", "3", "4", "5", "6", "7", "8", "9" }
-- Wibar:
beautiful.bar_height   = dpi(24)
beautiful.bar_position = "top"
-- Set the background:
beautiful.background_dot_tile_size = dpi(100)
beautiful.background_dot_width     = dpi(6)
if not is_laptop then
	awful.screen.connect_for_each_screen(function(s)
		require("backgrounds.dots")(s)
	end)
end
awful.util.shell = "/bin/bash"
-- }}}
-- Menubar {{{
menubar.prompt_args = {
	bg_cursor = xrdb.background,
}
-- }}}
-- Additional Configurations {{{
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
require("awful.autofocus")
require("appearance.behaviors.sloppyfocus")
require("appearance.behaviors.attachbelow") -- attachbelow patch
require("appearance.behaviors.smartborders") -- smartborder patch
require("appearance.behaviors.shadow_for_floating") -- shadows on floating windows only
require("appearance.titlebars")
-- }}}
-- # vim:foldmethod=marker:foldlevel=0
