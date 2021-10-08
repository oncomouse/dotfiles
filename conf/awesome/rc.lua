-- luacheck: globals awesome client io tag screen
-- Includes {{{
pcall(require, "luarocks.loader")
-- Update package.path
local addtional_path_prefix = os.getenv("HOME") .. "/dotfiles/conf/awesome/"
local addtional_path = ";" .. addtional_path_prefix .. "?/init.lua;" .. addtional_path_prefix .. "?.lua"
package.path = package.path .. addtional_path
local gears = require("gears")
local awful = require("awful")
local beautiful = require("beautiful")
require("awful.autofocus")
-- Rules
-- local ruled = require("ruled")
-- Notification library
local naughty = require("naughty")
-- Hotkeys
-- local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
-- require("awful.hotkeys_popup.keys")
-- }}}
-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
naughty.connect_signal("request::display_error", function(message, startup)
	naughty.notification({
		urgency = "critical",
		title = "Oops, an error happened" .. (startup and " during startup!" or "!"),
		message = message,
	})
end)
-- }}}
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
beautiful.layout_centeredmonocle = gears.color.recolor_image(
	gears.filesystem.get_themes_dir() .. "default/layouts/magnifierw.png",
	beautiful.fg_normal
)
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
-- This is used later as the default terminal and editor to run.
beautiful.terminal = "kitty"
-- Default modkey.
beautiful.modkey = "Mod4"
-- Table of layouts to cover with awful.layout.inc, order matters.
beautiful.default_layouts = {
	awful.layout.suit.tile.right,
	awful.layout.suit.tile.left,
	require("layouts.centeredmonocle"),
	awful.layout.suit.floating,
}
-- }}}
-- Widgets: {{{
-- Track named widgets for key press events
local dotfiles_target = os.getenv("DOTFILES_TARGET") or "desktop"
local is_laptop = (dotfiles_target == "laptop")

beautiful.wibar_widgets = is_laptop
		and {
			{ "dwmblocks-volume.sh", 30, "volume" },
			{ "dwmblocks-brightness.sh", 30, "brightness" },
			{ "dwmblocks-battery.sh", 30, "battery" },
			{ "dwmblocks-date.sh", 5, "date" },
		}
	or {
		{ "dwmblocks-volume.sh", 30, "volume" },
		{ "dwmblocks-mpris.sh", 30, "mpris" },
		{ "dwmblocks-weather.sh", 600, "weather" },
		{ "dwmblocks-date.sh", 5, "date" },
	}
-- }}}
-- Wibar {{{
-- Mouse behavior for layoutbox:
beautiful.layoutbox_mousebuttons = {
	awful.button({}, 1, function()
		awful.layout.inc(1)
	end),
	awful.button({}, 3, function()
		awful.layout.inc(-1)
	end),
	awful.button({}, 4, function()
		awful.layout.inc(1)
	end),
	awful.button({}, 5, function()
		awful.layout.inc(-1)
	end),
}
-- Mouse behavior for taglist:
beautiful.taglist_mousebuttons = {
	awful.button({}, 1, function(t)
		t:view_only()
	end),
	awful.button({ beautiful.modkey }, 1, function(t)
		if client.focus then
			client.focus:move_to_tag(t)
		end
	end),
	awful.button({}, 3, awful.tag.viewtoggle),
	awful.button({ beautiful.modkey }, 3, function(t)
		if client.focus then
			client.focus:toggle_tag(t)
		end
	end),
	awful.button({}, 4, function(t)
		awful.tag.viewprev(t.screen)
	end),
	awful.button({}, 5, function(t)
		awful.tag.viewnext(t.screen)
	end),
}
-- Mouse behavior for tasklist:
beautiful.tasklist_mousebuttons = {
	awful.button({}, 1, require("utils.swap_main")),
	awful.button({}, 3, function()
		awful.menu.client_list({ theme = { width = 250 } })
	end),
	awful.button({}, 4, function()
		awful.client.focus.byidx(1)
	end),
	awful.button({}, 5, function()
		awful.client.focus.byidx(-1)
	end),
}
beautiful.client_mousebuttons = {
	awful.button({}, 1, function(c)
		c:activate({ context = "mouse_click" })
	end),
	awful.button({ beautiful.modkey }, 1, function(c)
		c:activate({ context = "mouse_click", action = "mouse_move" })
	end),
	awful.button({ beautiful.modkey }, 3, function(c)
		c:activate({ context = "mouse_click", action = "mouse_resize" })
	end),
}
beautiful.sloppy_focus = true
-- }}}
-- Keybindings {{{
local dmenucmd = {
	"rofi",
	"-theme",
	"~/dotfiles/conf/rofi/barmenu.rasi",
	"-match",
	"fuzzy",
	"-auto-select",
	"-font",
	beautiful.font,
	"-show",
	"drun",
	"-show-icons",
	"-drun-display-format",
	"{name}",
}
local rofiwincmd = {
	"rofi",
	"-theme",
	"~/dotfiles/conf/rofi/barmenu.rasi",
	"-match",
	"fuzzy",
	"-auto-select",
	"-font",
	beautiful.font,
	"-show",
	"window",
	"-show-icons",
	"-window-format",
	"{w} {c} {t:25}",
}
local rofiemojicmd = {
	"rofi",
	"-show",
	"emoji",
	"-modi",
	"emoji",
	"-font",
	beautiful.font,
}
local rofinetworkcmd = {
	"networkmanager_dmenu",
	"-m",
	"1",
	"-fn",
	beautiful.font:gsub("%s(%d+)", ":size=%1"):gsub("%sNormal", "-Normal"),
	"-nb",
	xrdb.color0,
	"-nf",
	xrdb.color7,
	"-sb",
	xrdb.color6,
	"-sf",
	xrdb.color0,
}
local function sh_cmd(cmd)
	return function()
		awful.spawn({ awful.util.shell, "-c", cmd })
	end
end

awful.keyboard.append_global_keybindings({
	awful.key({ beautiful.modkey, "Mod1" }, "r", function()
		awful.spawn(dmenucmd)
	end, {
		description = "Drun menu",
		group = "Launcher",
	}),
	awful.key({ beautiful.modkey, "Shift" }, "n", function()
		awful.spawn(rofinetworkcmd)
	end, {
		description = "Networkmanager menu",
		group = "Launcher",
	}),
	awful.key({ beautiful.modkey, "Control" }, "space", function()
		awful.spawn(rofiemojicmd)
	end, {
		description = "Emoji menu",
		group = "Launcher",
	}),
	awful.key({ beautiful.modkey, "Shift" }, "w", function()
		awful.spawn(rofiwincmd)
	end, {
		description = "Window menu",
		group = "Launcher",
	}),
	awful.key({ beautiful.modkey, "Shift" }, "p", sh_cmd("dwm-powermenu.sh"), {
		description = "Powermenu",
		group = "Launcher",
	}),
})

awful.keyboard.append_global_keybindings({
	awful.key({ beautiful.modkey, "Shift" }, "Return", function()
		awful.spawn(beautiful.terminal)
	end, {
		description = "Open a Terminal",
		group = "Awesome",
	}),

	awful.key(
		{ beautiful.modkey, "Shift" },
		"b",
		require("widgets.make").keypress("dwm-brightness.sh default", "brightness"),
		{
			description = "Set Default Brightness",
			group = "Awesome",
		}
	),
	awful.key({ beautiful.modkey }, "b", function()
		for s in screen do
			s.wibar.visible = not s.wibar.visible
		end
	end, {
		description = "Toggle Bar Visibility",
		group = "Awesome",
	}),
	awful.key({ beautiful.modkey }, "q", awesome.restart, {
		description = "Reload Awesome",
		group = "Awesome",
	}),
	awful.key({ beautiful.modkey, "Shift" }, "q", awesome.quit, {
		description = "Quit Awesome",
		group = "Awesome",
	}),
})

awful.keyboard.append_global_keybindings({
	awful.key({ beautiful.modkey }, "j", function()
		awful.client.focus.byidx(1)
	end, {
		description = "Focus Next by Index",
		group = "Client",
	}),
	awful.key({ beautiful.modkey }, "k", function()
		awful.client.focus.byidx(-1)
	end, {
		description = "Focus Previous by Index",
		group = "Client",
	}),
	awful.key({ beautiful.modkey }, "l", function()
		awful.tag.incmwfact(0.05)
	end, {
		description = "increase master width factor",
		group = "Client",
	}),
	awful.key({ beautiful.modkey }, "h", function()
		awful.tag.incmwfact(-0.05)
	end, {
		description = "decrease master width factor",
		group = "Client",
	}),
	awful.key({ beautiful.modkey }, "i", function()
		awful.tag.incnmaster(1, nil, true)
	end, {
		description = "increase the number of master clients",
		group = "Client",
	}),
	awful.key({ beautiful.modkey }, "d", function()
		awful.tag.incnmaster(-1, nil, true)
	end, {
		description = "decrease the number of master clients",
		group = "Client",
	}),
	awful.key({ beautiful.modkey }, "Tab", function()
		awful.client.focus.history.previous()
		if client.focus then
			client.focus:raise()
		end
	end, {
		description = "View Last Tag",
		group = "Client",
	}),
})

local function move_resize(move)
	return function(c)
		if c.floating then
			c:relative_move(move.x or 0, move.y or 0, move.w or 0, move.h or 0)
		end
	end
end

client.connect_signal("request::default_keybindings", function()
	awful.keyboard.append_client_keybindings({
		awful.key({ beautiful.modkey }, "Return", require("utils.swap_main"), {
			description = "Zoom",
			group = "Client",
		}),
		awful.key({ beautiful.modkey, "Shift" }, "c", function(c)
			c:kill()
		end, {
			description = "Close Client",
			group = "Client",
		}),
		awful.key(
			{ beautiful.modkey },
			"space",
			awful.client.floating.toggle,
			{ description = "Toggle Floating", group = "Client" }
		),
		awful.key({ beautiful.modkey }, "Up", move_resize({ y = -25 }), {
			description = "Move Up",
			group = "Client",
		}),
		awful.key({ beautiful.modkey }, "Down", move_resize({ y = 25 }), {
			description = "Move Down",
			group = "Client",
		}),
		awful.key({ beautiful.modkey }, "Right", move_resize({ x = 25 }), {
			description = "Move Right",
			group = "Client",
		}),
		awful.key({ beautiful.modkey }, "Left", move_resize({ x = -25 }), {
			description = "Move Left",
			group = "Client",
		}),
		awful.key({ beautiful.modkey, "Shift" }, "Up", move_resize({ h = -25 }), {
			description = "Grow Height",
			group = "Client",
		}),
		awful.key({ beautiful.modkey, "Shift" }, "Down", move_resize({ h = 25 }), {
			description = "Grow Height",
			group = "Client",
		}),
		awful.key({ beautiful.modkey, "Shift" }, "Right", move_resize({ w = 25 }), {
			description = "Grow Width",
			group = "Client",
		}),
		awful.key({ beautiful.modkey, "Shift" }, "Left", move_resize({ w = -25 }), {
			description = "Shrink Width",
			group = "Client",
		}),
	})
end)

awful.keyboard.append_global_keybindings({
	awful.key({ beautiful.modkey }, "m", function()
		awful.layout.set(require("layouts.centeredmonocle"))
	end, {
		description = "select centered monocle layout",
		group = "Layout",
	}),
	awful.key({ beautiful.modkey, "Shift" }, "m", function()
		awful.layout.set(awful.layout.suit.max)
	end, {
		description = "select max layout",
		group = "Layout",
	}),
	awful.key({ beautiful.modkey }, "t", function()
		awful.layout.set(awful.layout.suit.tile.right)
	end, {
		description = "select tiled layout",
		group = "Layout",
	}),
	awful.key({ beautiful.modkey, "Shift" }, "t", function()
		awful.layout.set(awful.layout.suit.tile.left)
	end, {
		description = "select tiled layout",
		group = "Layout",
	}),
})

awful.keyboard.append_global_keybindings({
	awful.key({
		modifiers = { beautiful.modkey },
		keygroup = "numrow",
		description = "View Single Tag",
		group = "Tag",
		on_press = function(index)
			local screen = awful.screen.focused()
			local tag = screen.tags[index]
			if tag then
				tag:view_only()
			end
		end,
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Control" },
		keygroup = "numrow",
		description = "Toggle Tag Visibility",
		group = "Tag",
		on_press = function(index)
			local screen = awful.screen.focused()
			local tag = screen.tags[index]
			if tag then
				awful.tag.viewtoggle(tag)
			end
		end,
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Shift" },
		keygroup = "numrow",
		description = "Move Focused Client to Tag",
		group = "Tag",
		on_press = function(index)
			if client.focus then
				local tag = client.focus.screen.tags[index]
				if tag then
					client.focus:move_to_tag(tag)
				end
			end
		end,
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Control", "Shift" },
		keygroup = "numrow",
		description = "Toggle Focused Client on Tag",
		group = "Tag",
		on_press = function(index)
			if client.focus then
				local tag = client.focus.screen.tags[index]
				if tag then
					client.focus:toggle_tag(tag)
				end
			end
		end,
	}),
})

awful.keyboard.append_global_keybindings({
	awful.key({}, "XF86KbdBrightnessDown", sh_cmd("sudo /usr/local/bin/keyboard-backlight down")),
	awful.key({}, "XF86KbdBrightnessUp", sh_cmd("sudo /usr/local/bin/keyboard-backlight up")),
	awful.key({}, "XF86MonBrightnessUp", require("widgets.make").keypress("dwm-brightness.sh up", "brightness")),
	awful.key({}, "XF86MonBrightnessDown", require("widgets.make").keypress("dwm-brightness.sh down", "brightness")),
	awful.key({}, "XF86AudioMute", require("widgets.make").keypress("liskin-media mute", "volume")),
	awful.key({}, "XF86AudioLowerVolume", require("widgets.make").keypress("liskin-media volume down", "volume")),
	awful.key({}, "XF86AudioRaiseVolume", require("widgets.make").keypress("liskin-media volume up", "volume")),
	awful.key({}, "XF86AudioPlay", require("widgets.make").keypress("liskin-media play", "mpris")),
	awful.key({}, "XF86AudioPrev", require("widgets.make").keypress("liskin-media prev", "mpris")),
	awful.key({}, "XF86AudioNext", require("widgets.make").keypress("liskin-media next", "mpris")),
	awful.key({}, "XF86AudioStop", require("widgets.make").keypress("liskin-media stop", "mpris")),
})
-- }}}
-- Mouse: {{{
-- }}}
require("smartborders")
-- Attach default layouts
tag.connect_signal("request::default_layouts", function()
	awful.layout.append_default_layouts(beautiful.default_layouts)
end)
-- Client mouse behavior
client.connect_signal("mouse::enter", function(c) -- Sloppy focus
	c:emit_signal("request::activate", "mouse_enter", { raise = not beautiful.sloppy_focus })
end)
client.connect_signal("request::default_mousebindings", function()
	awful.mouse.append_client_mousebindings(beautiful.client_mousebuttons)
end)
require("bar")
-- vim: foldlevel=0:foldmethod=marker
