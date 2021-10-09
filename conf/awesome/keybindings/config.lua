-- luacheck: globals awesome screen client
local beautiful = require("beautiful")
local xrdb = beautiful.xresources.get_current_theme()
local awful = require("awful")
local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")
-- This is used later as the default terminal and editor to run.
beautiful.terminal = "kitty"
-- Default modkey.
beautiful.modkey = "Mod4"

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
local dmenu_font = 	beautiful.font:gsub("%s(%d+)", ":size=%1"):gsub("%sNormal", "-Normal")
local rofinetworkcmd = {
	"networkmanager_dmenu",
	"-m",
	"1",
	"-fn",
	dmenu_font,
	"-nb",
	xrdb.color0,
	"-nf",
	xrdb.color7,
	"-sb",
	xrdb.color6,
	"-sf",
	xrdb.color0,
}
local powermenucmd = {
	"awesome-powermenu.sh",
	"-font",
	beautiful.font,
}
beautiful.global_keybindings = {
	awful.key({
		modifiers = { beautiful.modkey, "Mod1" },
		key = "r",
		on_press = function()
			awful.spawn(dmenucmd)
		end,
		description = "Drun menu",
		group = "Launcher",
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Shift" },
		key = "n",
		on_press = function()
			awful.spawn(rofinetworkcmd)
		end,
		description = "Networkmanager menu",
		group = "Launcher",
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Control" },
		key = "space",
		on_press = function()
			awful.spawn(rofiemojicmd)
		end,
		description = "Emoji menu",
		group = "Launcher",
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Shift" },
		key = "w",
		on_press = function()
			awful.spawn(rofiwincmd)
		end,
		description = "Window menu",
		group = "Launcher",
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Shift" },
		key = "p",
		on_press = function() awful.spawn(powermenucmd) end,
		description = "Powermenu",
		group = "Launcher",
	}),

	awful.key({
		modifiers = { beautiful.modkey, "Shift" },
		key = "Return",
		on_press = function()
			awful.spawn(beautiful.terminal)
		end,
		description = "Open a Terminal",
		group = "Awesome",
	}),

	awful.key({
		modifiers = { beautiful.modkey, "Shift" },
		key = "b",
		on_press = require("widgets.make").keypress("dwm-brightness.sh default", "brightness"),
		description = "Set Default Brightness",
		group = "Awesome",
	}),
	awful.key({
		modifiers = { beautiful.modkey },
		key = "b",
		on_press = function()
			for s in screen do
				s.wibar.visible = not s.wibar.visible
			end
		end,
		description = "Toggle Bar Visibility",
		group = "Awesome",
	}),
	awful.key({
		modifiers = { beautiful.modkey },
		key = "q",
		on_press = awesome.restart,
		description = "Reload Awesome",
		group = "Awesome",
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Shift" },
		key = "q",
		on_press = awesome.quit,
		description = "Quit Awesome",
		group = "Awesome",
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Shift" },
		key = "/",
		on_press = hotkeys_popup.show_help,
		description = "Show Hotkeys",
		group = "Awesome",
	}),
	awful.key({
		modifiers = { beautiful.modkey },
		key = "j",
		on_press = function()
			awful.client.focus.byidx(1)
		end,
		description = "Focus Next by Index",
		group = "Client",
	}),
	awful.key({
		modifiers = { beautiful.modkey },
		key = "k",
		on_press = function()
			awful.client.focus.byidx(-1)
		end,
		description = "Focus Previous by Index",
		group = "Client",
	}),
	awful.key({
		modifiers = { beautiful.modkey },
		key = "l",
		on_press = function()
			awful.tag.incmwfact(0.05)
		end,
		description = "increase master width factor",
		group = "Client",
	}),
	awful.key({
		modifiers = { beautiful.modkey },
		key = "h",
		on_press = function()
			awful.tag.incmwfact(-0.05)
		end,
		description = "decrease master width factor",
		group = "Client",
	}),
	awful.key({
		modifiers = { beautiful.modkey },
		key = "i",
		on_press = function()
			awful.tag.incnmaster(1, nil, true)
		end,
		description = "increase the number of master clients",
		group = "Client",
	}),
	awful.key({
		modifiers = { beautiful.modkey },
		key = "d",
		on_press = function()
			awful.tag.incnmaster(-1, nil, true)
		end,
		description = "decrease the number of master clients",
		group = "Client",
	}),
	awful.key({
		modifiers = { beautiful.modkey },
		key = "Tab",
		on_press = function()
			awful.client.focus.history.previous()
			if client.focus then
				client.focus:raise()
			end
		end,
		description = "View Last Tag",
		group = "Client",
	}),
	awful.key({
		modifiers = { beautiful.modkey },
		key = "m",
		on_press = function()
			awful.layout.set(require("layouts.centeredmonocle"))
		end,
		description = "select centered monocle layout",
		group = "Layout",
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Shift" },
		key = "m",
		on_press = function()
			awful.layout.set(awful.layout.suit.max)
		end,
		description = "select max layout",
		group = "Layout",
	}),
	awful.key({
		modifiers = { beautiful.modkey },
		key = "t",
		on_press = function()
			awful.layout.set(awful.layout.suit.tile.right)
		end,
		description = "select tiled layout",
		group = "Layout",
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Shift" },
		key = "t",
		on_press = function()
			awful.layout.set(awful.layout.suit.tile.left)
		end,
		description = "select tiled layout",
		group = "Layout",
	}),
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
	awful.key({
		modifiers = {},
		key = "XF86KbdBrightnessDown",
		on_press = require("keybindings.utils.sh_cmd")("sudo /usr/local/bin/keyboard-backlight down"),
		description = "Raise Keyboard Backlight",
		group = "Media",
	}),
	awful.key({
		modifiers = {},
		key = "XF86KbdBrightnessUp",
		on_press = require("keybindings.utils.sh_cmd")("sudo /usr/local/bin/keyboard-backlight up"),
		description = "Lower Keyboard Backlight",
		group = "Media",
	}),
	awful.key({
		modifiers = {},
		key = "XF86MonBrightnessUp",
		on_press = require("widgets.make").keypress("dwm-brightness.sh up", "brightness"),
		description = "Raise Brightness by 5%",
		group = "Media",
	}),
	awful.key({
		modifiers = {},
		key = "XF86MonBrightnessDown",
		on_press = require("widgets.make").keypress("dwm-brightness.sh down", "brightness"),
		description = "Lower Brightness by 5%",
		group = "Media",
	}),
	awful.key({
		modifiers = {},
		key = "XF86AudioMute",
		on_press = require("widgets.make").keypress("liskin-media mute", "volume"),
		description = "Toggle Mute",
		group = "Media",
	}),
	awful.key({
		modifiers = {},
		key = "XF86AudioLowerVolume",
		on_press = require("widgets.make").keypress("liskin-media volume down", "volume"),
		description = "Lower Volume by 5%",
		group = "Media",
	}),
	awful.key({
		modifiers = {},
		key = "XF86AudioRaiseVolume",
		on_press = require("widgets.make").keypress("liskin-media volume up", "volume"),
		description = "Raise Volume by 5%",
		group = "Media",
	}),
	awful.key({
		modifiers = {},
		key = "XF86AudioPlay",
		on_press = require("widgets.make").keypress("liskin-media play", "mpris"),
		description = "Play/pause Audio",
		group = "Media",
	}),
	awful.key({
		modifiers = {},
		key = "XF86AudioPrev",
		on_press = require("widgets.make").keypress("liskin-media prev", "mpris"),
		description = "Previous Track",
		group = "Media",
	}),
	awful.key({
		modifiers = {},
		key = "XF86AudioNext",
		on_press = require("widgets.make").keypress("liskin-media next", "mpris"),
		description = "Next Track",
		group = "Media",
	}),
	awful.key({
		modifiers = {},
		key = "XF86AudioStop",
		on_press = require("widgets.make").keypress("liskin-media stop", "mpris"),
		descriptino = "Stop Audio from Playing",
		group = "Media",
	}),
}

beautiful.client_keybindings = {
	awful.key({
		modifiers = { beautiful.modkey },
		key = "Return",
		on_press = require("utils.swap_main"),
		description = "Zoom",
		group = "Client",
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Shift" },
		key = "c",
		on_press = function(c)
			c:kill()
		end,
		description = "Close Client",
		group = "Client",
	}),
	awful.key({
		modifiers = { beautiful.modkey },
		key = "space",
		on_press = awful.client.floating.toggle,
		description = "Toggle Floating",
		group = "Client",
	}),
	awful.key({
		modifiers = { beautiful.modkey },
		keygroup = "arrows",
		on_press = function(direction, client)
			local direction_to_move = {
				Up = { y = -25 },
				Down = { y = 25 },
				Left = { x = -25 },
				Right = { x = 25 },
			}
			require("keybindings.utils.moveresize")(direction_to_move[direction])(client)
		end
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Shift" },
		keygroup = "arrows",
		on_press = function(direction, client)
			local direction_to_move = {
				Up = { h = -25 },
				Down = { h = 25 },
				Left = { w = -25 },
				Right = { w = 25 },
			}
			require("keybindings.utils.moveresize")(direction_to_move[direction])(client)
		end
	}),
}
-- }}}


