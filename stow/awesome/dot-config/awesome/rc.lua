-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
local xrdb = beautiful.xresources.get_current_theme()
-- Make dpi function global
-- dpi = beautiful.xresources.apply_dpi
-- Make xresources colors global
x = {
    --           xrdb variable
    background = xrdb.background,
    foreground = xrdb.foreground,
    color0     = xrdb.color0,
    color1     = xrdb.color1,
    color2     = xrdb.color2,
    color3     = xrdb.color3,
    color4     = xrdb.color4,
    color5     = xrdb.color5,
    color6     = xrdb.color6,
    color7     = xrdb.color7,
    color8     = xrdb.color8,
    color9     = xrdb.color9,
    color10    = xrdb.color10,
    color11    = xrdb.color11,
    color12    = xrdb.color12,
    color13    = xrdb.color13,
    color14    = xrdb.color14,
    color15    = xrdb.color15,
}
-- Notification library
local naughty = require("naughty")
-- local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup")
-- Custom playerctl guts:
local playerctl = require("signals.playerctl")
playerctl.enable()
local volume_widget_factory = require("volume")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")
local layout_cm = require("layouts.centeredmonocle")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
	naughty.notify({
		preset = naughty.config.presets.critical,
		title = "Oops, there were errors during startup!",
		text = awesome.startup_errors,
	})
end

-- Handle runtime errors after startup
do
	local in_error = false
	awesome.connect_signal("debug::error", function(err)
		-- Make sure we don't go into an endless error loop
		if in_error then return end
		in_error = true

		naughty.notify({
			preset = naughty.config.presets.critical,
			title = "Oops, an error happened!",
			text = tostring(err),
		})
		in_error = false
	end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
beautiful.init(gears.filesystem.get_themes_dir() .. "xresources/theme.lua")
beautiful.layout_centeredmonocle =
	gears.color.recolor_image(
		gears.filesystem.get_themes_dir() .. "default/layouts/maxw.png",
		beautiful.fg_normal
	)
beautiful.useless_gap = 0
beautiful.border_normal = x.color8
beautiful.border_focus = x.color7
beautiful.font = "FantasqueSansMono Nerd Font Normal 16"
beautiful.tasklist_disable_icon = true
beautiful.widget_space = {
	left = nil,
	right = " ⁞ "
}
beautiful.hotkeys_modifiers_fg = x.color4
beautiful.hotkeys_font = "FiraCode Nerd Font Normal 16"
beautiful.hotkeys_description_font = "FiraCode Nerd Font Normal 12"

-- This is used later as the default terminal and editor to run.
terminal = "kitty"
editor = os.getenv("EDITOR") or "nvim"
editor_cmd = terminal .. " -e " .. editor
-- require("widgets.exit_screen")

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts
= {
	awful.layout.suit.tile,
	layout_cm,
	awful.layout.suit.max,
	awful.layout.suit.floating,
	-- awful.layout.suit.tile.left,
	-- awful.layout.suit.tile.bottom,
	-- awful.layout.suit.tile.top,
	-- awful.layout.suit.fair,
	-- awful.layout.suit.fair.horizontal,
	-- awful.layout.suit.spiral,
	-- awful.layout.suit.spiral.dwindle,
	-- awful.layout.suit.max,
	-- awful.layout.suit.max.fullscreen,
	-- awful.layout.suit.magnifier,
	-- awful.layout.suit.corner.nw,
	-- awful.layout.suit.corner.ne,
	-- awful.layout.suit.corner.sw,
	-- awful.layout.suit.corner.se,
}
-- }}}

-- {{{ Menu
-- Menubar configuration
-- menubar.show_categories = false
-- menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- Mpris player status:
mpris_widget = wibox.widget{
	{
		{
			id = "status",
			text = "栗",
			widget = wibox.widget.textbox,
		},
		{
			id = "title",
			text = "",
			widget = wibox.widget.textbox,
		},
		layout = wibox.layout.fixed.horizontal,
	},
	margin2 = 1,
	widget = wibox.container.margin,
}
mpris_widget:connect_signal("button::press", function(_, _, _, button)
	if button == 1 then
		playerctl.play()
	elseif button == 2 then
		playerctl.previous_track()
	elseif button == 3 then
		playerctl.next_track()
	end
end)
function truncate(st, len)
	if string.len(st) > len then
		return string.sub(st, 0, len - 1) .. "…"
	else
		return st
	end
end
awesome.connect_signal("dotfiles::playerctl::stopped", function()
	mpris_widget:get_children_by_id("status")[1]:set_text("栗")
	mpris_widget:get_children_by_id("title")[1]:set_text("")
end)
awesome.connect_signal("dotfiles::playerctl::status", function(playing)
	local status
	if playing == "play" then
		status = "契 "
	elseif playing == "pause" then
		status = " "
	else
		status = "栗 "
	end
	mpris_widget:get_children_by_id("status")[1]:set_text(status)
end)
awesome.connect_signal("dotfiles::playerctl::title_artist_album", function(
title,
	artist,
	_
)
	mpris_widget:get_children_by_id("title")[1]:set_text(
		truncate(string.format("%s - %s", artist, title), 30)
	)
end)
-- Volume Icon:
volume_widget = volume_widget_factory()
volume_widget:enable()
volume_widget:connect_signal("button::press", function()
	volume_widget:mute()
end)

-- {{{ Wibar
-- Create a textclock widget
mytextclock = wibox.widget.textclock(" %a %l:%M %p ")
mytextclock:connect_signal("button::press", function()
	awful.spawn.easy_async('date +"  %A, %B %d %Y"', function(stdout)
		naughty.notify({
			title = "Today's Date",
			text = string.gsub(stdout, "^%s*(.-)%s*$", "%1"),
		})
	end)
end)

-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
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
	end)
)

local tasklist_buttons = gears.table.join(
	awful.button({}, 1, function(c)
		if c == awful.client.getmaster() then
			awful.client.swap.byidx(1, c)
		else
			c:swap(awful.client.getmaster())
		end
	end),
	awful.button({}, 3, function()
		awful.menu.client_list({
			theme = { width = 250 },
		})
	end),
	awful.button({}, 4, function()
		awful.client.focus.byidx(1)
	end),
	awful.button({}, 5, function()
		awful.client.focus.byidx(-1)
	end)
)

gears.wallpaper.set(awesome.xrdb_get_value("", "background"))

awful.screen.connect_for_each_screen(function(s)
	-- Each screen has its own tag table.
	awful.tag(
		{ "1", "2", "3", "4", "5", "6", "7", "8", "9" },
		s,
		awful.layout.layouts[1]
	)
	-- Create a promptbox for each screen
	s.mypromptbox = awful.widget.prompt()
	-- Create an imagebox widget which will contain an icon indicating which layout we're using.
	-- We need one layoutbox per screen.
	s.mylayoutbox = awful.widget.layoutbox(s)
	s.mylayoutbox:buttons(
		gears.table.join(
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
			end)
		)
	)
	-- Create a taglist widget
	s.mytaglist = awful.widget.taglist{
		screen = s,
		filter = awful.widget.taglist.filter.all,
		buttons = taglist_buttons,
	}

	-- Create a tasklist widget
	s.mytasklist = awful.widget.tasklist{
		screen = s,
		filter = awful.widget.tasklist.filter.focused,
		buttons = tasklist_buttons,
	}
	-- Create the wibox
	s.mywibox = awful.wibar({
		position = "top",
		screen = s,
		height = "24",
	})

	-- Add widgets to the wibox -- Left widgets -- Middle widget -- Right widgets
	s.mywibox:setup{
		layout = wibox.layout.align.horizontal,
		{
			spacing = 5,
			spacing_widget = beautiful.widget_space.left ~= nil and {
				text = beautiful.widget_space.left,
				widget = wibox.widget.textbox,
			} or nil,
			layout = wibox.layout.fixed.horizontal,
			s.mytaglist,
			s.mylayoutbox,
			s.mypromptbox,
		},
		s.mytasklist,
		{
			layout = wibox.layout.fixed.horizontal,
			spacing_widget = beautiful.widget_space.right ~= nil and {
				text = beautiful.widget_space.right,
				widget = wibox.widget.textbox,
			} or nil,
			spacing = 20,
			volume_widget,
			mpris_widget,
			mytextclock,
		},
	}
end)
-- }}}

-- {{{ Mouse bindings
-- root.buttons(
-- 	gears.table.join(
-- 		awful.button({}, 3, function()
-- 			mymainmenu:toggle()
-- 		end),
-- 		awful.button({}, 4, awful.tag.viewnext),
-- 		awful.button({}, 5, awful.tag.viewprev)
-- 	)
-- )
-- }}}

-- {{{ Key bindings
globalkeys = gears.table.join(
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
	awful.key({ modkey }, "Escape", awful.tag.history.restore, {
		description = "go back",
		group = "tag",
	}),

	awful.key(
		{ modkey },
		"j",
		function()
			awful.client.focus.byidx(1)
		end,
		{
			description = "focus next by index",
			group = "client",
		}
	),
	awful.key(
		{ modkey },
		"k",
		function()
			awful.client.focus.byidx(-1)
		end,
		{
			description = "focus previous by index",
			group = "client",
		}
	),

	-- Layout manipulation
	awful.key(
		{ modkey, "Shift" },
		"j",
		function()
			awful.client.swap.byidx(1)
		end,
		{
			description = "swap with next client by index",
			group = "client",
		}
	),
	awful.key(
		{ modkey, "Shift" },
		"k",
		function()
			awful.client.swap.byidx(-1)
		end,
		{
			description = "swap with previous client by index",
			group = "client",
		}
	),
	awful.key(
		{ modkey, "Control" },
		"j",
		function()
			awful.screen.focus_relative(1)
		end,
		{
			description = "focus the next screen",
			group = "screen",
		}
	),
	awful.key(
		{ modkey, "Control" },
		"k",
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
	awful.key(
		{ modkey },
		"Tab",
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

	-- Standard program
	awful.key(
		{ modkey, "Shift" },
		"Return",
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

	awful.key(
		{ modkey },
		"l",
		function()
			awful.tag.incmwfact(0.05)
		end,
		{
			description = "increase master width factor",
			group = "layout",
		}
	),
	awful.key(
		{ modkey },
		"h",
		function()
			awful.tag.incmwfact(-0.05)
		end,
		{
			description = "decrease master width factor",
			group = "layout",
		}
	),
	awful.key(
		{ modkey, "Shift" },
		"h",
		function()
			awful.tag.incnmaster(1, nil, true)
		end,
		{
			description = "increase the number of master clients",
			group = "layout",
		}
	),
	awful.key(
		{ modkey, "Shift" },
		"l",
		function()
			awful.tag.incnmaster(-1, nil, true)
		end,
		{
			description = "decrease the number of master clients",
			group = "layout",
		}
	),
	awful.key(
		{ modkey, "Control" },
		"h",
		function()
			awful.tag.incncol(1, nil, true)
		end,
		{
			description = "increase the number of columns",
			group = "layout",
		}
	),
	awful.key(
		{ modkey, "Control" },
		"l",
		function()
			awful.tag.incncol(-1, nil, true)
		end,
		{
			description = "decrease the number of columns",
			group = "layout",
		}
	),
	awful.key(
		{ modkey },
		"space",
		function()
			awful.layout.inc(1)
		end,
		{
			description = "select next",
			group = "layout",
		}
	),
	awful.key(
		{ modkey, "Shift" },
		"space",
		function()
			awful.layout.inc(-1)
		end,
		{
			description = "select previous",
			group = "layout",
		}
	),
	-- Switch layouts by name:
	awful.key(
		{ modkey },
		"m",
		function()
			awful.layout.set(layout_cm)
		end,
		{
			description = "select centered monocle layout",
			group = "layout",
		}
	),
	awful.key(
		{ modkey, "Shift" },
		"m",
		function()
			awful.layout.set(awful.layout.suit.max)
		end,
		{
			description = "select max layout",
			group = "layout",
		}
	),
	awful.key(
		{ modkey },
		"t",
		function()
			awful.layout.set(awful.layout.suit.tile)
		end,
		{
			description = "select tiled layout",
			group = "layout",
		}
	),

	awful.key(
		{ modkey, "Control" },
		"n",
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

	-- Prompt
	awful.key(
		{ modkey },
		"r",
		function()
			awful.screen.focused().mypromptbox:run()
		end,
		{
			description = "run prompt",
			group = "launcher",
		}
	),

	awful.key(
		{ modkey },
		"x",
		function()
			awful.prompt.run{
				prompt = "Run Lua code: ",
				textbox = awful.screen.focused().mypromptbox.widget,
				exe_callback = awful.util.eval,
				history_path = awful.util.get_cache_dir() .. "/history_eval",
			}
		end,
		{
			description = "lua execute prompt",
			group = "awesome",
		}
	),
	-- Menubar
	awful.key(
		{ modkey },
		"p",
		function()
			awful.spawn.with_shell("rofi -show combi -match fuzzy -show-icons")
		end,
		{
			description = "show the menubar",
			group = "launcher",
		}
	),
	awful.key(
		{ modkey, "Shift" },
		"p",
		function()
			awful.spawn.with_shell(
				"~/dotfiles/scripts/rofi/powermenu/powermenu.sh"
			)
		end,
		{
			description = "show main menu",
			group = "launcher",
		}
	),
	-- awful.key({ modkey, "Shift" }, "p", function () exit_screen_show() end,
	-- 		  {description = "show main menu", group = "launcher"}),

	-- Print Screen
	awful.key({}, "Print", function()
		awful.spawn.with_shell(
			"sleep 0.2 && " .. "scrot ~/'Seafile/My Library/Photos/Screenshots/%Y-%m-%d_%H%M%S-$wx$h_scrot.png'"
		)
	end),
	awful.key({ "Shift" }, "Print", function()
		awful.spawn.with_shell(
			"sleep 0.2 && " .. "scrot -s ~/'Seafile/My Library/Photos/Screenshots/%Y-%m-%d_%H%M%S-$wx$h_scrot.png'"
		)
	end),

	-- Media Keys
	awful.key({}, "XF86AudioPlay", playerctl.play),
	awful.key({}, "XF86AudioStop", playerctl.stop),
	awful.key({}, "XF86AudioPrev", playerctl.previous_track),
	awful.key({}, "XF86AudioNext", playerctl.next_track),
	awful.key({}, "XF86AudioLowerVolume", function()
		volume_widget:down()
	end),
	awful.key({}, "XF86AudioRaiseVolume", function()
		volume_widget:up()
	end),
	awful.key({}, "XF86AudioMute", function()
		volume_widget:mute()
	end)
)
clientkeys = gears.table.join(
	--,
	-- awful.key({ modkey,		   }, "m",
	-- 	function (c)
	-- 		c.maximized = not c.maximized
	-- 		c:raise()
	-- 	end ,
	-- 	{description = "(un)maximize", group = "client"}),
	-- awful.key({ modkey, "Control" }, "m",
	-- 	function (c)
	-- 		c.maximized_vertical = not c.maximized_vertical
	-- 		c:raise()
	-- 	end ,
	-- 	{description = "(un)maximize vertically", group = "client"}),
	-- awful.key({ modkey, "Shift"   }, "m",
	-- 	function (c)
	-- 		c.maximized_horizontal = not c.maximized_horizontal
	-- 		c:raise()
	-- 	end ,
	-- 	{description = "(un)maximize horizontally", group = "client"})
	awful.key(
		{ modkey },
		"f",
		function(c)
			c.fullscreen = not c.fullscreen
			c:raise()
		end,
		{
			description = "toggle fullscreen",
			group = "client",
		}
	),
	awful.key(
		{ modkey, "Shift" },
		"c",
		function(c)
			c:kill()
		end,
		{
			description = "close",
			group = "client",
		}
	),
	awful.key({ modkey, "Control" }, "space", awful.client.floating.toggle, {
		description = "toggle floating",
		group = "client",
	}),
	awful.key(
		{ modkey },
		"Return",
		function(c)
			c:swap(awful.client.getmaster())
		end,
		{
			description = "move to master",
			group = "client",
		}
	),
	awful.key(
		{ modkey },
		"o",
		function(c)
			c:move_to_screen()
		end,
		{
			description = "move to screen",
			group = "client",
		}
	),
	-- awful.key({ modkey,		   }, "t",	  function (c) c.ontop = not c.ontop			end,
	-- 		  {description = "toggle keep on top", group = "client"}),
	awful.key(
		{ modkey },
		"n",
		function(c)
			-- The client currently has the input focus, so it cannot be
			-- minimized, since minimized clients can't have the focus.
			c.minimized = true
		end,
		{
			description = "minimize",
			group = "client",
		}
	)
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
	globalkeys = gears.table.join(
		-- Toggle tag on focused client.
		globalkeys,
		-- View tag only.
		awful.key(
			{ modkey },
			"#" .. i + 9,
			function()
				local screen = awful.screen.focused()
				local tag = screen.tags[i]
				if tag then
					tag:view_only()
				end
			end,
			{
				description = "view tag #" .. i,
				group = "tag",
			}
		),
		-- Toggle tag display.
		awful.key(
			{ modkey, "Control" },
			"#" .. i + 9,
			function()
				local screen = awful.screen.focused()
				local tag = screen.tags[i]
				if tag then
					awful.tag.viewtoggle(tag)
				end
			end,
			{
				description = "toggle tag #" .. i,
				group = "tag",
			}
		),
		-- Move client to tag.
		awful.key(
			{ modkey, "Shift" },
			"#" .. i + 9,
			function()
				if client.focus then
					local tag = client.focus.screen.tags[i]
					if tag then
						client.focus:move_to_tag(tag)
					end
				end
			end,
			{
				description = "move focused client to tag #" .. i,
				group = "tag",
			}
		),
		awful.key(
			{ modkey, "Control", "Shift" },
			"#" .. i + 9,
			function()
				if client.focus then
					local tag = client.focus.screen.tags[i]
					if tag then
						client.focus:toggle_tag(tag)
					end
				end
			end,
			{
				description = "toggle focused client on tag #" .. i,
				group = "tag",
			}
		)
	)
end

clientbuttons = gears.table.join(
	awful.button({}, 1, function(c)
		c:emit_signal("request::activate", "mouse_click", { raise = true })
	end),
	awful.button({ modkey }, 1, function(c)
		c:emit_signal("request::activate", "mouse_click", { raise = true })
		awful.mouse.client.move(c)
	end),
	awful.button({ modkey }, 3, function(c)
		c:emit_signal("request::activate", "mouse_click", { raise = true })
		awful.mouse.client.resize(c)
	end)
)

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = { -- All clients will match this rule.
{
	rule = {},
	properties = {
		border_width = beautiful.border_width,
		border_color = beautiful.border_normal,
		focus = awful.client.focus.filter,
		raise = true,
		keys = clientkeys,
		buttons = clientbuttons,
		screen = awful.screen.preferred,
		placement = awful.placement.no_overlap + awful.placement.no_offscreen,
	},
}, {
	-- Floating clients.
	rule_any = {
		instance = { "DTA", "copyq", "pinentry" }, -- Firefox addon DownThemAll. -- Includes session name in class.
		class = { -- "Arandr",
		-- "Blueman-manager",
		"feh", "Gimp", "Thunar", "xtightvncviewer" }, -- "Sxiv", -- "MessageWin",  -- kalarm. -- "Kruler", -- "Gpick", -- "veromix", -- "Wpa_gui", -- "Tor Browser", -- Needs a fixed window size to avoid fingerprinting by screen size.
		-- Note that the name property shown in xprop might be set slightly after creation of the client
		-- and the name shown there might not match defined rules here.
		name = { "Event Tester" }, -- xev.
		role = { "AlarmWindow", "ConfigManager", "pop-up" }, -- Thunderbird's calendar. -- Thunderbird's about:config. -- e.g. Google Chrome's (detached) Developer Tools.
	},
	properties = { floating = true },
}, {
	-- Add titlebars to normal clients and dialogs
	rule_any = {
		type = { "normal", "dialog" },
	},
	properties = { titlebars_enabled = false },
}, {
	rule = { class = "zoom" },
	properties = {
		screen = 1,
		tag = "7",
	},
}, {
	rule = { class = "Zotero" },
	properties = {
		screen = 1,
		tag = "8",
	},
} }

-- Set Firefox to always map on the tag named "2" on screen 1.
-- { rule = { class = "Firefox" },
--   properties = { screen = 1, tag = "2" } },
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

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
	-- buttons for the titlebar
	local buttons = gears.table.join(
		awful.button({}, 1, function()
			c:emit_signal("request::activate", "titlebar", { raise = true })
			awful.mouse.client.move(c)
		end),
		awful.button({}, 3, function()
			c:emit_signal("request::activate", "titlebar", { raise = true })
			awful.mouse.client.resize(c)
		end)
	)
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
	c:emit_signal("request::activate", "mouse_enter", { raise = false })
end)
-- Source: https://github.com/actionless/awesome_config/blob/master/actionless/util/tag.lua
function get_tiled(t)
	local tiled_clients = {}

	-- @TODO: add some fix for sticky clients: DONE?
	if not t then
		return tiled_clients
	end
	local s = t.screen
	if s.selected_tags and #s.selected_tags > 1 then
		return s.tiled_clients
	end

	local visible_clients = s.tiled_clients
	local clients_on_tag = t:clients()
	for _, c in pairs(visible_clients) do
		if c.valid and c.sticky then
			table.insert(tiled_clients, c)
		end
	end
	for _, c in pairs(clients_on_tag) do
		if not c.floating and not c.fullscreen and not c.maximized_vertical and not c.maximized_horizontal and not c.minimized and not c.sticky then
			table.insert(tiled_clients, c)
		end
	end
	return tiled_clients
end
client.connect_signal("focus", function(c)
	c.border_color = beautiful.border_focus
end)
client.connect_signal("unfocus", function(c)
	c.border_color = beautiful.border_normal
end)
-- No borders if only client in tile mode:
function update_borders(c)
	update_tag_borders(c.first_tag or awful.screen.focused().selected_tag)
end
function update_tag_borders(t)
	local tags = awful.screen.focused().selected_tags
	local clients = {}
	for _, t in pairs(tags) do
		for _, c in pairs(get_tiled(t)) do
			table.insert(clients, c)
		end
	end
	local b = beautiful.border_width
	if #clients == 1 then
		b = 0
	end
	for _, c in pairs(clients) do
		c.border_width = b
	end
end
client.connect_signal("manage", update_borders)
client.connect_signal("unmanage", update_borders)
client.connect_signal("tagged", update_borders)
client.connect_signal("untagged", update_borders)
tag.connect_signal("property::layout", update_tag_borders)
tag.connect_signal("property::selected", update_tag_borders)
-- }}}
