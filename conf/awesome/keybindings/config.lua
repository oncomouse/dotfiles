-- luacheck: globals awesome screen client
local beautiful = require("beautiful")
local awful = require("awful")
local gears = require("gears")
local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")
-- This is used later as the default terminal and editor to run.
beautiful.terminal = "kitty"
beautiful.file_explorer = "thunar"
-- Default modkey.
beautiful.modkey = "Mod4"
beautiful.global_keybindings = {}
-- Should moveresize and moveresizeedge toggle floating?
beautiful.toggle_floating_on_moveresize = true
-- Launcher Commands {{{

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
	"-location",
	"1",
	"-theme-str",
	"window { width: 100%; }",
	"-font",
	beautiful.font,
}
local rofinetworkcmd = {
	"networkmanager_dmenu",
	"-location",
	"1",
	"-theme-str",
	"window { width: 100%; }",
	"-font",
	beautiful.font,
}
local rofimusiccmd = {
	"rofimusic.sh",
	beautiful.font,
}
-- }}}
-- Launcher Keybinding Group {{{
beautiful.global_keybindings = gears.table.join(beautiful.global_keybindings, {
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
		modifiers = { beautiful.modkey, "Mod1" },
		key = "p",
		on_press = function()
			awful.spawn(rofimusiccmd)
		end,
		description = "Music menu",
		group = "Launcher",
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Mod1" },
		key = "c",
		on_press = function()
			local cp = require("widgets.clock_popup")
			cp:toggle()
		end,
		description = "Display Clock",
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
		key = "l",
		on_press = require("layouts.launcher"),
		description = "Layout menu",
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
		on_press = require("utils.powermenu"),
		description = "Powermenu",
		group = "Launcher",
	}),
})
-- }}}
-- Awesome Keybinding Group {{{
beautiful.global_keybindings = gears.table.join(beautiful.global_keybindings, {
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
		modifiers = { beautiful.modkey },
		key = "e",
		on_press = function()
			awful.spawn(beautiful.file_explorer)
		end,
		description = "Open the File Explorer",
		group = "Awesome",
	}),

	awful.key({
		modifiers = { beautiful.modkey, "Shift" },
		key = "b",
		on_press = require("widgets.keypress")("dwm-brightness.sh default", "brightness"),
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
})
-- }}}
-- Client Keybinding Group {{{
beautiful.global_keybindings = gears.table.join(beautiful.global_keybindings, {
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
		modifiers = { beautiful.modkey, "Shift" },
		key = "j",
		on_press = function()
			awful.client.swap.byidx(1)
		end,
		description = "Swap Client with Next by Index",
		group = "Client",
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Shift" },
		key = "k",
		on_press = function()
			awful.client.swap.byidx(-1)
		end,
		description = "Swap Client with Previous by Index",
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
		key = "u",
		on_press = awful.client.urgent.jumpto,
		description = "Jump to Urgent Client",
		group = "Client",
	}),
})
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
		key = "f",
		description = "Toggle Fullscreen",
		group = "Client",
		on_press = function(c)
			c.fullscreen = not c.fullscreen
			c:raise()
		end,
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Shift" },
		key = "l",
		on_press = function(c)
			awful.client.incwfact(0.05, c)
		end,
		description = "increase window factor",
		group = "Client",
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Shift" },
		key = "h",
		on_press = function(c)
			awful.client.incwfact(-0.05, c)
		end,
		description = "decrease window factor",
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
		end,
		description = "Move Client by 25px",
		group = "Client",
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
		end,
		description = "Resize Client by 25px",
		group = "Client",
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Control" },
		keygroup = "arrows",
		on_press = function(direction, client)
			require("keybindings.utils.moveresizeedge")(direction)(client)
		end,
		description = "Move Client to Edge",
		group = "Client",
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Control", "Shift" },
		keygroup = "arrows",
		on_press = function(direction, client)
			require("keybindings.utils.moveresizeedge")(direction, true)(client)
		end,
		description = "Resize Client to Edge",
		group = "Client",
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Control" },
		key = "c",
		on_press = require("keybindings.utils.centerclient"),
		description = "Center Floating Client",
		group = "Client",
	}),
}
-- }}}
-- Layout Keybinding Group {{{
beautiful.global_keybindings = gears.table.join(beautiful.global_keybindings, {
	awful.key({
		modifiers = { beautiful.modkey, "Shift" },
		key = "t",
		on_press = function()
			awful.layout.set(awful.layout.suit.tile.left)
		end,
		description = "Select Tiled Layout",
		group = "Layout",
	}),
	awful.key({
		modifiers = { beautiful.modkey },
		key = "m",
		on_press = function()
			awful.layout.set(require("layouts.centeredmonocle"))
		end,
		description = "Select Centered Monocle Layout",
		group = "Layout",
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Shift" },
		key = "m",
		on_press = function()
			awful.layout.set(awful.layout.suit.max)
		end,
		description = "Select Monocle Layout",
		group = "Layout",
	}),
	awful.key({
		modifiers = { beautiful.modkey },
		key = "t",
		on_press = function()
			awful.layout.set(awful.layout.suit.tile.right)
		end,
		description = "Select Right Tile Layout",
		group = "Layout",
	}),
})
-- }}}
-- Tag Keybinding Group {{{
beautiful.global_keybindings = gears.table.join(beautiful.global_keybindings, {
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
-- }}}
-- Media Keybindings Group {{{
beautiful.global_keybindings = gears.table.join(beautiful.global_keybindings, {
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
		on_press = require("widgets.keypress")("dwm-brightness.sh up", "brightness"),
		description = "Raise Brightness by 5%",
		group = "Media",
	}),
	awful.key({
		modifiers = {},
		key = "XF86MonBrightnessDown",
		on_press = require("widgets.keypress")("dwm-brightness.sh down", "brightness"),
		description = "Lower Brightness by 5%",
		group = "Media",
	}),
	awful.key({
		modifiers = {},
		key = "XF86AudioMute",
		on_press = require("widgets.keypress")("liskin-media mute", "volume"),
		description = "Toggle Mute",
		group = "Media",
	}),
	awful.key({
		modifiers = {},
		key = "XF86AudioLowerVolume",
		on_press = require("widgets.keypress")("liskin-media volume down", "volume"),
		description = "Lower Volume by 5%",
		group = "Media",
	}),
	awful.key({
		modifiers = {},
		key = "XF86AudioRaiseVolume",
		on_press = require("widgets.keypress")("liskin-media volume up", "volume"),
		description = "Raise Volume by 5%",
		group = "Media",
	}),
	awful.key({
		modifiers = {},
		key = "XF86AudioPlay",
		on_press = function() awesome.emit_signal("widget::mpris::action", "play_pause") end,
		description = "Play/pause Audio",
		group = "Media",
	}),
	awful.key({
		modifiers = {},
		key = "XF86AudioPrev",
		on_press = function() awesome.emit_signal("widget::mpris::action", "previous") end,
		description = "Previous Track",
		group = "Media",
	}),
	awful.key({
		modifiers = {},
		key = "XF86AudioNext",
		on_press = function() awesome.emit_signal("widget::mpris::action", "next") end,
		description = "Next Track",
		group = "Media",
	}),
	awful.key({
		modifiers = {},
		key = "XF86AudioStop",
		on_press = function() awesome.emit_signal("widget::mpris::action", "stop") end,
		description = "Stop Audio from Playing",
		group = "Media",
	}),
	awful.key({
		modifiers = {},
		key = "XF86Eject",
		on_press = function()
			awful.spawn("eject -T")
		end,
		description = "Eject CD-ROM",
		group = "Media",
	}),
	awful.key({
		modifiers = { beautiful.modkey },
		key = "Print",
		on_press = function()
			awful.spawn.with_shell(
				"scrot ~/Seadrive/My\\ Libraries/My\\ Library/Photos/Screenshots/'%Y-%m-%d-%H%M%S_$wx$h.png'"
			)
		end,
		description = "Take a Screenshot",
		group = "Media",
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Shift" },
		key = "Print",
		on_press = function()
			awful.spawn.with_shell(
				"scrot -s ~/Seadrive/My\\ Libraries/My\\ Library/Photos/Screenshots/'%Y-%m-%d-%H%M%S_$wx$h.png'"
			)
		end,
		description = "Take an Interactive Screenshot",
		group = "Media",
	}),
	awful.key({
		modifiers = { beautiful.modkey, "Shift", "Ctrl" },
		key = "Print",
		on_press = function()
			awful.spawn.with_shell("xcolor | xclip -selection clipboard")
		end,
		description = "Onscreen Colour Picker",
		group = "Media",
	})
})
-- }}}
-- vim:foldmethod=marker:foldlevel=0
