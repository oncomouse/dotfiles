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
-- Widget and layout library
local wibox = require("wibox")
-- Notification library
local naughty = require("naughty")
-- Hotkeys
-- local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
-- require("awful.hotkeys_popup.keys")
local layout_cm = require("layouts.centeredmonocle")
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
-- Utilities: {{{
local swap_main = require("utils.swap_main")
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
local apply_background = require("backgrounds.dots")
awful.screen.connect_for_each_screen(function(s)
	apply_background(s)
end)
awful.util.shell = "/bin/bash"
-- This is used later as the default terminal and editor to run.
local terminal = "kitty"
-- Default modkey.
local modkey = "Mod4"
-- Table of layouts to cover with awful.layout.inc, order matters.
tag.connect_signal("request::default_layouts", function()
	awful.layout.append_default_layouts({
		awful.layout.suit.tile.right,
		awful.layout.suit.tile.left,
		layout_cm,
		awful.layout.suit.floating,
	})
end)
-- }}}
-- Widgets: {{{
-- Track named widgets for key press events
local widget_signals = {}
local function block_watcher(cmd, delay, name)
	local widget = awful.widget.watch(cmd, delay)
	-- Trigger for button presses
	widget:connect_signal("button::press", function(_, _, _, button)
		awful.spawn.easy_async_with_shell("env BUTTON=" .. button .. " " .. cmd, function(stdout)
			widget:set_text(stdout)
		end)
	end)
	if name then
		widget_signals[name] = widget
		-- Internal keypress handler:
		widget:connect_signal("widget::update", function()
			awful.spawn.easy_async_with_shell(cmd, function(stdout)
				widget:set_text(stdout)
			end)
		end)
	end
	return widget
end

-- System-wide signal dispersal for key presses
awesome.connect_signal("widget::update", function(name)
	if widget_signals[name] then
		local widget = widget_signals[name]
		widget:emit_signal("widget::update")
	end
end)

-- Handler for key presses
local function media_key_press(cmd, signal)
	return function()
		awful.spawn.easy_async_with_shell(cmd, function()
			awesome.emit_signal("widget::update", signal)
		end)
	end
end

local dotfiles_target = os.getenv("DOTFILES_TARGET") or "desktop"
local is_laptop = (dotfiles_target == "laptop")

local function make_wibar_widgets(widget_definitions)
	local widgets = wibox.widget({
		layout = wibox.layout.fixed.horizontal,
		spacing = 10,
		spacing_widget = {
			color = xrdb.color15,
			text = " ",
			widget = wibox.widget.textbox,
		},
		widget = wibox.container.place,
	})

	for _, widget in ipairs(widget_definitions) do
		table.insert(widgets.children, block_watcher(widget[1], widget[2], widget[3]))
	end
	return widgets
end
beautiful.wibar_widgets = make_wibar_widgets(is_laptop and {
	{ "dwmblocks-volume.sh", 30, "volume" },
	{ "dwmblocks-brightness.sh", 30, "brightness" },
	{ "dwmblocks-battery.sh", 30, "battery" },
	{ "dwmblocks-date.sh", 5, "date" },
} or {
	{ "dwmblocks-volume.sh", 30, "volume" },
	{ "dwmblocks-mpris.sh", 30, "mpris" },
	{ "dwmblocks-weather.sh", 600, "weather" },
	{ "dwmblocks-date.sh", 5, "date" },
})
-- }}}
-- Wibar {{{
screen.connect_signal("request::desktop_decoration", function(s)
	for _, t in ipairs(beautiful.tags) do
		awful.tag.add(t, {
			screen = s,
			layout = awful.layout.layouts[1],
			master_width_factor = beautiful.mfact,
		})
	end
	awful.screen.focused().tags[1]:view_only()
	s.layoutbox = awful.widget.layoutbox(s)
	s.layoutbox:buttons({
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
	})
	s.layoutbox = wibox.container.margin(s.layoutbox, 4, 4, 4, 4)
	-- s.mylayoutbox.forced_width = tonumber(last(gears.string.split(beautiful.font, " "))) + 4
	s.taglist = awful.widget.taglist({
		screen = s,
		filter = awful.widget.taglist.filter.noempty,
		buttons = {
			awful.button({}, 1, function(t)
				t:view_only()
			end),
			awful.button({ modkey }, 1, function(t)
				if client.focus then
					client.focus:move_to_tag(t)
				end
			end),
			awful.button({}, 3, awful.tag.viewtoggle),
			awful.button({ modkey }, 3, function(t)
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
		},
	})
	s.tasklist = awful.widget.tasklist({
		screen = s,
		filter = awful.widget.tasklist.filter.focused,
		buttons = {
			awful.button({}, 1, swap_main),
			awful.button({}, 3, function()
				awful.menu.client_list({ theme = { width = 250 } })
			end),
			awful.button({}, 4, function()
				awful.client.focus.byidx(1)
			end),
			awful.button({}, 5, function()
				awful.client.focus.byidx(-1)
			end),
		},
		widget_template = {
			{
				{
					{
						id = "text_role",
						widget = wibox.widget.textbox,
					},
					layout = wibox.layout.fixed.horizontal,
				},
				left = 10,
				right = 10,
				widget = wibox.container.margin,
			},
			id = "background_role",
			widget = wibox.container.background,
		},
	})

	s.wibar = awful.wibar({
		position = beautiful.bar_position,
		screen = s,
		height = beautiful.bar_height,
		visible = true,
	})
	s.wibar.widget = {
		layout = wibox.layout.align.horizontal,
		{
			spacing = 1,
			layout = wibox.layout.fixed.horizontal,
			s.taglist,
			{
				{ widget = s.layoutbox },
				widget = wibox.container.place,
			},
		},
		s.tasklist,
		beautiful.wibar_widgets,
	}
end)
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
	awful.key({ modkey, "Mod1" }, "r", function()
		awful.spawn(dmenucmd)
	end, {
		description = "Drun menu",
		group = "Launcher",
	}),
	awful.key({ modkey, "Shift" }, "n", function()
		awful.spawn(rofinetworkcmd)
	end, {
		description = "Networkmanager menu",
		group = "Launcher",
	}),
	awful.key({ modkey, "Control" }, "space", function()
		awful.spawn(rofiemojicmd)
	end, {
		description = "Emoji menu",
		group = "Launcher",
	}),
	awful.key({ modkey, "Shift" }, "w", function()
		awful.spawn(rofiwincmd)
	end, {
		description = "Window menu",
		group = "Launcher",
	}),
	awful.key({ modkey, "Shift" }, "p", sh_cmd("dwm-powermenu.sh"), {
		description = "Powermenu",
		group = "Launcher",
	}),
})

awful.keyboard.append_global_keybindings({
	awful.key({ modkey, "Shift" }, "Return", function()
		awful.spawn(terminal)
	end, {
		description = "Open a Terminal",
		group = "Awesome",
	}),

	awful.key({ modkey, "Shift" }, "b", media_key_press("dwm-brightness.sh default", "brightness"), {
		description = "Set Default Brightness",
		group = "Awesome",
	}),
	awful.key({ modkey }, "b", function()
		for s in screen do
			s.wibar.visible = not s.wibar.visible
		end
	end, {
		description = "Toggle Bar Visibility",
		group = "Awesome",
	}),
	awful.key({ modkey }, "q", awesome.restart, {
		description = "Reload Awesome",
		group = "Awesome",
	}),
	awful.key({ modkey, "Shift" }, "q", awesome.quit, {
		description = "Quit Awesome",
		group = "Awesome",
	}),
})

awful.keyboard.append_global_keybindings({
	awful.key({ modkey }, "j", function()
		awful.client.focus.byidx(1)
	end, {
		description = "Focus Next by Index",
		group = "Client",
	}),
	awful.key({ modkey }, "k", function()
		awful.client.focus.byidx(-1)
	end, {
		description = "Focus Previous by Index",
		group = "Client",
	}),
	awful.key({ modkey }, "l", function()
		awful.tag.incmwfact(0.05)
	end, {
		description = "increase master width factor",
		group = "Client",
	}),
	awful.key({ modkey }, "h", function()
		awful.tag.incmwfact(-0.05)
	end, {
		description = "decrease master width factor",
		group = "Client",
	}),
	awful.key({ modkey }, "i", function()
		awful.tag.incnmaster(1, nil, true)
	end, {
		description = "increase the number of master clients",
		group = "Client",
	}),
	awful.key({ modkey }, "d", function()
		awful.tag.incnmaster(-1, nil, true)
	end, {
		description = "decrease the number of master clients",
		group = "Client",
	}),
	awful.key({ modkey }, "Tab", function()
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
		awful.key({ modkey }, "Return", swap_main, {
			description = "Zoom",
			group = "Client",
		}),
		awful.key({ modkey, "Shift" }, "c", function(c)
			c:kill()
		end, {
			description = "Close Client",
			group = "Client",
		}),
		awful.key(
			{ modkey },
			"space",
			awful.client.floating.toggle,
			{ description = "Toggle Floating", group = "Client" }
		),
		awful.key({ modkey }, "Up", move_resize({ y = -25 }), {
			description = "Move Up",
			group = "Client",
		}),
		awful.key({ modkey }, "Down", move_resize({ y = 25 }), {
			description = "Move Down",
			group = "Client",
		}),
		awful.key({ modkey }, "Right", move_resize({ x = 25 }), {
			description = "Move Right",
			group = "Client",
		}),
		awful.key({ modkey }, "Left", move_resize({ x = -25 }), {
			description = "Move Left",
			group = "Client",
		}),
		awful.key({ modkey, "Shift" }, "Up", move_resize({ h = -25 }), {
			description = "Grow Height",
			group = "Client",
		}),
		awful.key({ modkey, "Shift" }, "Down", move_resize({ h = 25 }), {
			description = "Grow Height",
			group = "Client",
		}),
		awful.key({ modkey, "Shift" }, "Right", move_resize({ w = 25 }), {
			description = "Grow Width",
			group = "Client",
		}),
		awful.key({ modkey, "Shift" }, "Left", move_resize({ w = -25 }), {
			description = "Shrink Width",
			group = "Client",
		}),
	})
end)

awful.keyboard.append_global_keybindings({
	awful.key({ modkey }, "m", function()
		awful.layout.set(layout_cm)
	end, {
		description = "select centered monocle layout",
		group = "Layout",
	}),
	awful.key({ modkey, "Shift" }, "m", function()
		awful.layout.set(awful.layout.suit.max)
	end, {
		description = "select max layout",
		group = "Layout",
	}),
	awful.key({ modkey }, "t", function()
		awful.layout.set(awful.layout.suit.tile.right)
	end, {
		description = "select tiled layout",
		group = "Layout",
	}),
	awful.key({ modkey, "Shift" }, "t", function()
		awful.layout.set(awful.layout.suit.tile.left)
	end, {
		description = "select tiled layout",
		group = "Layout",
	}),
})

awful.keyboard.append_global_keybindings({
	awful.key({
		modifiers = { modkey },
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
		modifiers = { modkey, "Control" },
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
		modifiers = { modkey, "Shift" },
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
		modifiers = { modkey, "Control", "Shift" },
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
	awful.key({}, "XF86MonBrightnessUp", media_key_press("dwm-brightness.sh up", "brightness")),
	awful.key({}, "XF86MonBrightnessDown", media_key_press("dwm-brightness.sh down", "brightness")),
	awful.key({}, "XF86AudioMute", media_key_press("liskin-media mute", "volume")),
	awful.key({}, "XF86AudioLowerVolume", media_key_press("liskin-media volume down", "volume")),
	awful.key({}, "XF86AudioRaiseVolume", media_key_press("liskin-media volume up", "volume")),
	awful.key({}, "XF86AudioPlay", media_key_press("liskin-media play", "mpris")),
	awful.key({}, "XF86AudioPrev", media_key_press("liskin-media prev", "mpris")),
	awful.key({}, "XF86AudioNext", media_key_press("liskin-media next", "mpris")),
	awful.key({}, "XF86AudioStop", media_key_press("liskin-media stop", "mpris")),
})
-- }}}
-- Mouse: {{{
client.connect_signal("mouse::enter", function(c) -- Sloppy focus
	c:emit_signal("request::activate", "mouse_enter", { raise = false })
end)
client.connect_signal("request::default_mousebindings", function()
	awful.mouse.append_client_mousebindings({
		awful.button({}, 1, function(c)
			c:activate({ context = "mouse_click" })
		end),
		awful.button({ modkey }, 1, function(c)
			c:activate({ context = "mouse_click", action = "mouse_move" })
		end),
		awful.button({ modkey }, 3, function(c)
			c:activate({ context = "mouse_click", action = "mouse_resize" })
		end),
	})
end)
-- }}}
-- Smartborders https://gist.github.com/ndgnuh/3cd462c634b6ac87ccfa6204127ac3bf {{{
local function make_border_dwim(t)
	-- use this because there might be multiple tag selected
	local cs = t.screen.clients
	local border = {}

	-- because maximized and fullscreen client
	-- are considered floating
	local function truefloat(c)
		return c.floating and not c.maximized and not c.fullscreen
	end

	-- TODO: look up table instead of ifelse
	if t.layout.name == "max" then
		for _, c in ipairs(cs) do
			if truefloat(c) then
				border[c] = true
			else
				border[c] = false
			end
		end
	elseif t.layout.name == "floating" then
		for _, c in ipairs(cs) do
			if c.maximized or c.fullscreen then
				border[c] = false
			else
				border[c] = true
			end
		end
	else
		local count = 0
		local only = nil
		for _, c in ipairs(cs) do
			if truefloat(c) then
				border[c] = true
			elseif c.maximized or c.fullscreen or c.minimized then
				border[c] = false
			else
				count = count + 1
				only = c
				border[c] = true
			end
		end
		if count == 1 then
			border[only] = false
		end
	end

	for c, bd in pairs(border) do
		if bd then
			c.border_width = beautiful.border_width
		else
			c.border_width = 0
		end
	end
end

local function make_border_dwim_screen_wrapper(s)
	local t = s.selected_tag
	-- no tag selected
	if t == nil then
		return
	end
	make_border_dwim(t)
end

local function make_border_dwim_client_wrapper(c)
	-- don't use c.first_tag or c.tags because unmanaged client have no tag
	-- so if either of those used, in a tile layout
	-- when a client is killed and there is one left, the border will still be there
	local s = c.screen
	if s then
		make_border_dwim_screen_wrapper(s)
	end
end

client.connect_signal("tagged", make_border_dwim_client_wrapper)
client.connect_signal("untagged", make_border_dwim_client_wrapper)
client.connect_signal("unmanage", make_border_dwim_client_wrapper)
client.connect_signal("property::floating", make_border_dwim_client_wrapper)
client.connect_signal("property::maximized", make_border_dwim_client_wrapper)
client.connect_signal("property::fullscreen", make_border_dwim_client_wrapper)
client.connect_signal("property::minimized", make_border_dwim_client_wrapper)
tag.connect_signal("property::layout", make_border_dwim)
screen.connect_signal("tag::history::update", make_border_dwim_screen_wrapper)
-- }}}
-- vim: foldlevel=0:foldmethod=marker
