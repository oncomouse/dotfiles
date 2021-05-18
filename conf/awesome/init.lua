-- luacheck: globals awesome client io tag screen
-- Includes {{{
-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
local beautiful = require("beautiful")
require("awful.autofocus")
-- Rules
local ruled = require("ruled")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local mpris_widget = require("widgets.mpris")
local volume_widget = require("widgets.volume")
local clock_widget = require("widgets.clock")
local brightness_widget = require("awesome-wm-widgets.brightness-widget.brightness")
-- Notification library
local naughty = require("naughty")
-- Hotkeys
local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")
-- Centered monocle mode:
local layout_cm = require("layouts.centeredmonocle")
-- Only show borders on tags with more than two clients:
require("utils.solo-noborder")
-- Swap with main client, or swap main client:
local swap_main = require("utils.swap_main")
-- Rofi utilities:
local rofi = require("utils.rofi")
-- Used for run menu:
local vi_parse = require("utils.vi_parse")
local revelation = require("awesome-revelation")
-- Heartbeat timer for caffeinating AwesomeWM:
local heartbeat = require("utils.heartbeat")
require("utils.border_gradient")
-- Clone Required Git Repos:
local download_libraries = require("utils.download_libraries")
download_libraries{
	"https://raw.githubusercontent.com/rxi/json.lua/master/json.lua",
	"streetturtle/awesome-wm-widgets",
	"guotsuan/awesome-revelation",
}
-- }}}
-- Startup {{{
awful.util.shell = "/usr/bin/bash"
revelation.init()
heartbeat.init()
-- }}}
-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
naughty.connect_signal("request::display_error", function(message, startup)
	naughty.notification {
		urgency = "critical",
		title   = "Oops, an error happened"..(startup and " during startup!" or "!"),
		message = message
	}
end)
-- }}}
-- {{{ Appearance
-- Set the background:
local apply_background = require('backgrounds.dots')
apply_background()
-- This is used later as the default terminal and editor to run.
terminal = "kitty"
-- Default modkey.
modkey = "Mod4"
-- Table of layouts to cover with awful.layout.inc, order matters.
tag.connect_signal("request::default_layouts", function()
	awful.layout.append_default_layouts{
		awful.layout.suit.tile.right,
		awful.layout.suit.tile.left,
		layout_cm,
	}
end)
-- }}}
-- {{{ Wibar
screen.connect_signal("request::desktop_decoration", function(s)
	awful.tag(
		{ "1", "2", "3", "4", "5", "6", "7", "8", "9" },
		s,
		awful.layout.layouts[1]
	)
	local atextbox = wibox.widget.textbox()
	s.mypromptbox = awful.widget.prompt{
		prompt = "<b>Run: </b>",
		hooks = { { {}, "Return", function(cmd)
			if not cmd or cmd:sub(1, 1) ~= ":" then
				return cmd
			end
			-- awful.prompt.run -- adjs, count, command -- adjs, count, command
			local act, cmd2 = cmd:gmatch(":([a-zA-Z1-9]+)[ ]+(.*)")()
			if not act then
				return cmd
			end
			return vi_parse(act, cmd2)
		end } },
		textbox = atextbox,
		history_path = gears.filesystem.get_cache_dir() .. "/history",
		exe_callback = function(cmd)
			awful.spawn(cmd)
		end,
	}
	s.mylayoutbox = awful.widget.layoutbox(s)
	s.mylayoutbox:buttons({
		awful.button({}, 1, function() awful.layout.inc(1) end),
		awful.button({}, 3, function() awful.layout.inc(-1) end),
		awful.button({}, 4, function() awful.layout.inc(1) end),
		awful.button({}, 5, function() awful.layout.inc(-1) end)
	})
	s.mytaglist = awful.widget.taglist{
		screen = s,
		filter = awful.widget.taglist.filter.all,
		buttons = {
			awful.button({}, 1, function(t) t:view_only() end),
			awful.button({ modkey }, 1, function(t) if client.focus then client.focus:move_to_tag(t) end end),
			awful.button({}, 3, awful.tag.viewtoggle),
			awful.button({ modkey }, 3, function(t) if client.focus then client.focus:toggle_tag(t) end end),
			awful.button({}, 4, function(t) awful.tag.viewprev(t.screen) end),
			awful.button({}, 5, function(t) awful.tag.viewnext(t.screen) end),
		},
	}

	s.mytasklist = awful.widget.tasklist{
		screen = s,
		filter = awful.widget.tasklist.filter.focused,
		buttons = {
			awful.button({}, 1, swap_main),
			awful.button({}, 3, function() awful.menu.client_list({ theme = { width = 250 }, }) end),
			awful.button({}, 4, function() awful.client.focus.byidx(1) end),
			awful.button({}, 5, function() awful.client.focus.byidx(-1) end),
		},
		widget_template = beautiful.tasklist_disable_icon and {
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
		} or nil,
	}
	s.mywibox = awful.wibar({
		position = beautiful.bar_position,
		screen = s,
		height = beautiful.bar_height,
	})

	s.mywibox.widget = {
		layout = wibox.layout.align.horizontal,
		{
			spacing = 10,
			layout = wibox.layout.fixed.horizontal,
			s.mytaglist,
			s.mylayoutbox,
			s.mypromptbox,
		},
		s.mytasklist,
		beautiful.wibar_right(),
	}
end)
-- }}}
-- {{{ Key bindings
awful.keyboard.append_global_keybindings({
	awful.key({ modkey, "Shift" }, "Return",
		function()
			awful.spawn(terminal)
		end,
		{
			description = "open a terminal",
			group = "launcher",
		}
	),
	awful.key({ modkey }, "q", awesome.restart, {
		description = "reload awesome",
		group = "awesome",
	}),
	awful.key({ modkey, "Shift" }, "q", awesome.quit, {
		description = "quit awesome",
		group = "awesome",
	}),
})
-- Tag Manipulation {{{
awful.keyboard.append_global_keybindings({
	awful.key({ modkey }, "s", hotkeys_popup.show_help, {
		description = "show help",
		group = "awesome",
	}),
	awful.key({ modkey }, "Left", awful.tag.viewprev, {
		description = "view previous",
		group = "tag",
	}),
	awful.key({ modkey }, "Right", awful.tag.viewnext, {
		description = "view next",
		group = "tag",
	}),
	awful.key({ modkey }, "j", function() awful.client.focus.byidx(1) end, {
		description = "focus next by index",
		group = "client",
	}),
	awful.key({ modkey }, "k", function() awful.client.focus.byidx(-1) end, {
		description = "focus previous by index",
		group = "client",
	}),
	awful.key({ modkey, "Shift" }, "j",
		function()
			awful.client.swap.byidx(1)
		end,
		{
			description = "swap with next client by index",
			group = "client",
		}
	),
	awful.key({ modkey, "Shift" }, "k",
		function()
			awful.client.swap.byidx(-1)
		end,
		{
			description = "swap with previous client by index",
			group = "client",
		}
	),
	awful.key({ modkey, "Control" }, "j",
		function()
			awful.screen.focus_relative(1)
		end,
		{
			description = "focus the next screen",
			group = "screen",
		}
	),
	awful.key({ modkey, "Control" }, "k",
		function()
			awful.screen.focus_relative(-1)
		end,
		{
			description = "focus the previous screen",
			group = "screen",
		}
	),
	awful.key({ modkey }, "u", awful.client.urgent.jumpto, {
		description = "jump to urgent client",
		group = "client",
	}),
	awful.key({ modkey, "Control" }, "m",
		function()
			local c = client.focus
			c.maximized_horizontal = false -- inserted
			c.maximized_vertical = false -- inserted
			c.maximized = not c.maximized
			c:raise()
		end,
		{
			description = "Maximize",
			group = "client",
		}
	),
	awful.key({ modkey }, "Tab",
		function()
			awful.client.focus.history.previous()
			if client.focus then
				client.focus:raise()
			end
		end,
		{
			description = "go back",
			group = "client",
		}
	),
})
-- }}}
-- Layout Manipulation {{{
awful.keyboard.append_global_keybindings({
	awful.key({ modkey }, "l",
		function()
			awful.tag.incmwfact(0.05)
		end,
		{
			description = "increase master width factor",
			group = "layout",
		}
	),
	awful.key({ modkey }, "h",
		function()
			awful.tag.incmwfact(-0.05)
		end,
		{
			description = "decrease master width factor",
			group = "layout",
		}
	),
	awful.key({ modkey }, "i",
		function()
			awful.tag.incnmaster(1, nil, true)
		end,
		{
			description = "increase the number of master clients",
			group = "layout",
		}
	),
	awful.key({ modkey }, "d",
		function()
			awful.tag.incnmaster(-1, nil, true)
		end,
		{
			description = "decrease the number of master clients",
			group = "layout",
		}
	),
	awful.key({ modkey, "Shift" }, "i",
		function()
			awful.tag.incncol(1, nil, true)
		end,
		{
			description = "increase the number of columns",
			group = "layout",
		}
	),
	awful.key({ modkey, "Shift" }, "d",
		function()
			awful.tag.incncol(-1, nil, true)
		end,
		{
			description = "decrease the number of columns",
			group = "layout",
		}
	),
})
-- }}}
-- Switch layouts by name {{{
awful.keyboard.append_global_keybindings({
	awful.key({ modkey }, "m",
		function()
			awful.layout.set(layout_cm)
		end,
		{
			description = "select centered monocle layout",
			group = "layout",
		}
	),
	awful.key({ modkey, "Shift" }, "m",
		function()
			awful.layout.set(awful.layout.suit.max)
		end,
		{
			description = "select max layout",
			group = "layout",
		}
	),
	awful.key({ modkey }, "t",
		function()
			awful.layout.set(awful.layout.suit.tile.right)
		end,
		{
			description = "select tiled layout",
			group = "layout",
		}
	),
	awful.key({ modkey, "Shift" }, "t",
		function()
			awful.layout.set(awful.layout.suit.tile.left)
		end,
		{
			description = "select tiled layout",
			group = "layout",
		}
	),

	awful.key({ modkey, "Control" }, "n",
		function()
			local c = awful.client.restore()
			-- Focus restored client
			if c then
				c:emit_signal(
					"request::activate",
					"key.unminimize",
					{ raise = true }
				)
			end
		end,
		{
			description = "restore minimized",
			group = "client",
		}
	),
})
-- }}}
-- Run prompts and menubars {{{
awful.keyboard.append_global_keybindings({
	awful.key({ modkey }, "r",
		function()
			awful.screen.focused().mypromptbox:run()
		end,
		{
			description = "run prompt",
			group = "launcher",
		}
	),

	awful.key({}, "F12", revelation, {
		description = "run revelation (exposé-like behavior)",
		group = "layout",
	}),

	awful.key({ modkey }, "x",
		function()
			awful.prompt.run{
				prompt = "<b>Run Lua code</b>: ",
				textbox = awful.screen.focused().mypromptbox.widget,
				exe_callback = function(s)
					-- Automatically load useful libraries:
					local code_string =
						[[
						local awful = require("awful")
						local gears = require("gears")
						local wibox = require("wibox")
					]] .. s
					awful.util.eval(code_string)
				end,
				history_path = awful.util.get_cache_dir() .. "/history_eval",
			}
		end,
		{
			description = "lua execute prompt",
			group = "launcher",
		}
	),
	awful.key({ modkey, "Mod1" }, "r", rofi.drun, {
		description = "show the menubar",
		group = "launcher",
	}),
	awful.key({ modkey }, "p", rofi.drun, {
		description = "show the menubar",
		group = "launcher",
	}),
	awful.key({ modkey, "Shift" }, "p", rofi.powermenu, {
		description = "show main menu",
		group = "launcher",
	}),
	awful.key({ modkey }, "w", rofi.window, {
		description = "show window menu",
		group = "launcher",
	}),
})
--- }}}
-- Print Screen {{{
awful.keyboard.append_global_keybindings({
	awful.key({}, "Print", function()
		awful.spawn.with_shell(
			"sleep 0.2 && " .. "scrot ~/'Seadrive/My Libraries/My Library/Photos/Screenshots/%Y-%m-%d_%H%M%S-$wx$h_scrot.png'"
		)
	end, {
		description = "take a screenshot",
		group = "media",
	}),
	awful.key({ "Shift" }, "Print", function()
		awful.spawn.with_shell(
			"sleep 0.2 && " .. "scrot -s ~/'Seadrive/My Libraries/My Library/Photos/Screenshots/%Y-%m-%d_%H%M%S-$wx$h_scrot.png'"
		)
	end, {
		description = "take a screenshot (select region)",
		group = "media",
	})
})
--- }}}
-- Media Keys {{{
awful.keyboard.append_global_keybindings({
	awful.key({}, "XF86MonBrightnessDown", function()
		brightness_widget:dec()
	end, {
		description = "lower monitor brightness",
		group = "media",
	}),
	awful.key({}, "XF86MonBrightnessUp", function()
		brightness_widget:inc()
	end, {
		description = "raise monitor brightness",
		group = "media",
	}),
	awful.key({}, "XF86AudioPlay", mpris_widget.play, {
		description = "play/pause audio",
		group = "media",
	}),
	awful.key({}, "XF86AudioStop", mpris_widget.stop, {
		description = "stop audio",
		group = "media",
	}),
	awful.key({}, "XF86AudioPrev", mpris_widget.previous_track, {
		description = "previous track",
		group = "media",
	}),
	awful.key({}, "XF86AudioNext", mpris_widget.next_track, {
		description = "next track",
		group = "media",
	}),
	awful.key({}, "XF86AudioLowerVolume", volume_widget.down, {
		description = "lower volume by 5%",
		group = "media",
	}),
	awful.key({}, "XF86AudioRaiseVolume", volume_widget.up, {
		description = "raise volume by 5%",
		group = "media",
	}),
	awful.key({}, "XF86AudioMute", volume_widget.mute, {
		description = "mute audio",
		group = "media",
	}),
	awful.key({ modkey, "Control" }, "d", clock_widget.toggle, {
		description = "show/hide calendar",
		group = "awesome",
	})
})
-- }}}
-- Number Key Bindings: {{{
awful.keyboard.append_global_keybindings({
	awful.key {
		modifiers   = { modkey },
		keygroup    = "numrow",
		description = "only view tag",
		group       = "tag",
		on_press    = function (index)
			local screen = awful.screen.focused()
			local tag = screen.tags[index]
			if tag then
				tag:view_only()
			end
		end,
	},
	awful.key {
		modifiers   = { modkey, "Control" },
		keygroup    = "numrow",
		description = "toggle tag",
		group       = "tag",
		on_press    = function (index)
			local screen = awful.screen.focused()
			local tag = screen.tags[index]
			if tag then
				awful.tag.viewtoggle(tag)
			end
		end,
	},
	awful.key {
		modifiers = { modkey, "Shift" },
		keygroup    = "numrow",
		description = "move focused client to tag",
		group       = "tag",
		on_press    = function (index)
			if client.focus then
				local tag = client.focus.screen.tags[index]
				if tag then
					client.focus:move_to_tag(tag)
				end
			end
		end,
	},
	awful.key {
		modifiers   = { modkey, "Control", "Shift" },
		keygroup    = "numrow",
		description = "toggle focused client on tag",
		group       = "tag",
		on_press    = function (index)
			if client.focus then
				local tag = client.focus.screen.tags[index]
				if tag then
					client.focus:toggle_tag(tag)
				end
			end
		end,
	},
	awful.key {
		modifiers   = { modkey },
		keygroup    = "numpad",
		description = "select layout directly",
		group       = "layout",
		on_press    = function (index)
			local t = awful.screen.focused().selected_tag
			if t then
				t.layout = t.layouts[index] or t.layout
			end
		end,
	}
})
-- }}}
-- Client keys & Buttons {{{
client.connect_signal("request::default_keybindings", function()
	awful.keyboard.append_client_keybindings({
		-- Flag menu for clients:
		awful.key({ modkey }, "f", rofi.client_flags, {
			description = "client flag menu",
			group = "client",
		}),
		-- Kill client:
		awful.key({ modkey, "Shift" }, "c", function(c) c:kill() end, {
			description = "close client",
			group = "client",
		}),
		-- Swap client with main:
		awful.key({ modkey }, "Return", swap_main, {
			description = "move to/from main",
			group = "client",
		})
	})
end)

client.connect_signal("request::default_mousebindings", function()
	awful.mouse.append_client_mousebindings({
		awful.button({ }, 1, function (c)
			c:activate { context = "mouse_click" }
		end),
		awful.button({ modkey }, 1, function (c)
			c:activate { context = "mouse_click", action = "mouse_move"  }
		end),
		awful.button({ modkey }, 3, function (c)
			c:activate { context = "mouse_click", action = "mouse_resize"}
		end),
	})
end)
-- }}}
-- }}}
-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
ruled.client.connect_signal("request::rules", function()
	-- All clients will match this rule.
	ruled.client.append_rule{
		rule = {},
		properties = {
			border_width = beautiful.border_width,
			border_color = beautiful.border_normal,
			focus = awful.client.focus.filter,
			raise = true,
			screen = awful.screen.preferred,
			placement = awful.placement.no_overlap + awful.placement.no_offscreen,
		},
	}
	-- Clients that float and have titlebars:
	ruled.client.append_rule{
		rule_any = {
			class = { "feh", "Gimp", "Thunar", "files", "Files", "Pcmanfm" },
		},
		properties = {
			floating = true,
			titlebars_enabled = true,
		},
	}
	-- Clients that have titlebars only:
	ruled.client.append_rule{
		rule_any = {
			class = { "libreoffice" },
		},
		properties = { titlebars_enabled = true },
	}
	-- Place file manager in the center of the screen:
	ruled.client.append_rule{
		rule_any = {
			class = { "Thunar", "files", "Files", "Pcmanfm" },
		},
		properties = {
			placement = awful.placement.centered + awful.placement.center_vertically,
			width = 1024,
			height = 615,
		},
	}
	-- Add titlebars to dialog clients
	ruled.client.append_rule{
		rule_any = {
			type = { "dialog" },
		},
		properties = { titlebars_enabled = true },
	}
	-- Attach Zoom to tag 7
	ruled.client.append_rule{
		rule = { class = "zoom" },
		properties = {
			screen = 1,
			tag = "7",
		},
	}
	-- Attach Zotero to tag 8
	ruled.client.append_rule{
		rule = { class = "Zotero" },
		properties = {
			screen = 1,
			tag = "8",
		},
	}
end)
-- }}}
-- Titlebars {{{
client.connect_signal("request::titlebars", function(c)
	-- buttons for the titlebar
	local buttons = {
		awful.button({}, 1, function() c:activate{
			context = "titlebar",
			action = "mouse_move",
		} end),
		-- Mod+3rd mouse raises client flag menu:
		awful.button({modkey}, 3, function() rofi.client_flags(c) end),
		awful.button({}, 3, function()
			c:activate{
				context = "titlebar",
				action = "mouse_resize",
			}
		end),
	}

	awful.titlebar(c).widget = {
		{
			-- Left
			awful.titlebar.widget.iconwidget(c),
			buttons = buttons,
			layout = wibox.layout.fixed.horizontal,
		},
		{
			-- Middle
			{
				-- Title
				align = "center",
				font = beautiful.titlebar_font,
				widget = awful.titlebar.widget.titlewidget(c),
			},
			buttons = buttons,
			layout = wibox.layout.flex.horizontal,
		},
		{
			-- Right
			-- Handle flags with Rofi, so we don't need these:
			-- awful.titlebar.widget.floatingbutton(c),
			-- awful.titlebar.widget.maximizedbutton(c),
			-- awful.titlebar.widget.stickybutton(c),
			-- awful.titlebar.widget.ontopbutton(c),
			awful.titlebar.widget.closebutton(c),
			layout = wibox.layout.fixed.horizontal(),
		},
		layout = wibox.layout.align.horizontal,
	}
end)
-- }}}
-- {{{ Notifications
ruled.notification.connect_signal('request::rules', function()
	-- All notifications will match this rule.
	ruled.notification.append_rule {
		rule       = { },
		properties = {
			screen           = awful.screen.preferred,
			implicit_timeout = 5,
		}
	}
end)

naughty.connect_signal("request::display", function(n)
	naughty.layout.box { notification = n }
end)
-- }}}
-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function(c)
	-- Set the windows at the slave,
	-- i.e. put it at the end of others instead of setting it master.
	if not awesome.startup then
		awful.client.setslave(c)
	end

	if awesome.startup and not c.size_hints.user_position and not c.size_hints.program_position then
		-- Prevent clients from being unreachable after screen count changes.
		awful.placement.no_offscreen(c)
	end
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
	c:emit_signal("request::activate", "mouse_enter", { raise = false })
end)

-- Set border_color to normal on unfocus (border gradient takes care of focus)
-- client.connect_signal("focus", function(c)
-- 	c.border_color = beautiful.border_focus
-- end)
client.connect_signal("unfocus", function(c)
	c.border_color = beautiful.border_normal
end)
-- }}}
-- vim: foldlevel=0:foldmethod=marker
