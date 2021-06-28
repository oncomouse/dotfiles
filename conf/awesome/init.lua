-- luacheck: globals awesome client io tag screen
-- Includes {{{
-- Clone Required Git Repos:
local download_libraries = require("utils.download_libraries")
download_libraries{
	"https://raw.githubusercontent.com/rxi/json.lua/master/json.lua",
	"streetturtle/awesome-wm-widgets",
	"guotsuan/awesome-revelation",
}
-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
local beautiful = require("beautiful")
require("awful.autofocus")
-- Rules
local ruled = require("ruled")
-- Widget and layout library
local wibox = require("wibox")
-- Notification library
local naughty = require("naughty")
-- Hotkeys
local hotkeys_popup = require("awful.hotkeys_popup")
-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")
-- Widgets:
local wifi_widget = require("widgets.wifi")
local volume_widget = require("widgets.volume")
local clock_widget = require("widgets.clock")
local mpris_widget = require("widgets.mpris")
local brightness_widget = require("widgets.brightness")
local battery_widget = require("widgets.battery")
local weather_widget = require("awesome-wm-widgets.weather-widget.weather")
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
-- Expose like behavior
local revelation = require("awesome-revelation")
-- Heartbeat timer for caffeinating AwesomeWM:
local heartbeat = require("utils.heartbeat")
-- Is this the laptop?
local is_laptop = require("utils.is_laptop")()
-- Animated Border Gradients:
require("utils.border_gradient")
-- Start our signal services:
local evil_init = require("evil")
-- Utilities:
local cairo = require("lgi").cairo
local function last(xs) return xs[#xs] end -- last element in a table
-- }}}
-- Startup {{{
awful.util.shell = "/usr/bin/bash"
revelation.init()
heartbeat.init()
evil_init()
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
-- Beautiful
beautiful.init(gears.filesystem.get_themes_dir().."xresources/theme.lua")
local xrdb = beautiful.xresources.get_current_theme()
local dpi = beautiful.xresources.apply_dpi
beautiful.icon_dir = os.getenv("HOME") .. "/.icons/oomox-xresources-reverse-flat/"
-- beautiful.font = "FiraCode Nerd Font Normal 11"
-- if is_laptop then
beautiful.font = "FiraCode Nerd Font Normal 10"
-- end
beautiful.notification_font = "FiraCode Nerd Font Normal 10"
beautiful.fg_icon = xrdb.color7
beautiful.icon_size = 16
beautiful.notification_icon_size = 16
beautiful.layout_centeredmonocle = gears.color.recolor_image(
	gears.filesystem.get_themes_dir() .. "default/layouts/magnifierw.png",
	beautiful.fg_normal
)
beautiful.useless_gap = 0 -- No gaps
beautiful.border_normal = xrdb.color8 -- Normal border color
beautiful.border_focus = xrdb.color1 -- Focused border color
beautiful.border_width = 2
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
local tag_width = dpi(tonumber(last(gears.string.split(beautiful.font, " "))) + 5)
local tag_bar_height = dpi(3)
local function taglist_squares_sel(w, h, fg)
	local img = cairo.ImageSurface(cairo.Format.ARGB32, w, h)
	local cr = cairo.Context(img)
	cr:set_source(gears.color(fg))
	cr:paint()
	return img
end
local function taglist_squares_unsel(w, h, fg)
    local img = cairo.ImageSurface(cairo.Format.ARGB32, w, h)
    local cr = cairo.Context(img)
    cr:set_source(gears.color(fg))
    cr:set_line_width(h/4)
    cr:rectangle(0, 0, w, h)
    cr:stroke()
    return img
end
beautiful.taglist_squares_sel = taglist_squares_sel(
	tag_width, tag_bar_height, xrdb.color6
)
beautiful.taglist_squares_unsel = taglist_squares_unsel(
	tag_width, tag_bar_height, xrdb.color15
)
beautiful.taglist_bg_focus = xrdb.color8
beautiful.taglist_fg_focus = xrdb.color15
-- Titlebar formatting:
beautiful.titlebar_font = "FiraCode Nerd Font Bold 12"
beautiful.titlebar_bg_normal = xrdb.color8
beautiful.titlebar_fg_normal = xrdb.color7
beautiful.titlebar_close_button_focus = gears.filesystem.get_themes_dir().."default/titlebar/close_normal.png"
beautiful.titlebar_close_button_focus  = gears.filesystem.get_themes_dir().."default/titlebar/close_focus.png"
beautiful.titlebar_bg_focus = xrdb.color0
beautiful.titlebar_fg_focus = xrdb.color15
-- Tasklist formatting:
beautiful.tasklist_disable_icon = true -- No icons in tasklist
beautiful.tasklist_bg_focus = xrdb.color0
beautiful.tasklist_fg_focus = xrdb.color7
beautiful.ow = {
	key=os.getenv("OW_KEY"),
	coordinates={
		tonumber(os.getenv("OW_LAT")),
		tonumber(os.getenv("OW_LONG")),
	}
}
-- Set the background:
beautiful.background_dot_tile_size = dpi(100)
beautiful.background_dot_width = dpi(6)
local apply_background = require('backgrounds.dots')
awful.screen.connect_for_each_screen(function(s)
	apply_background(s)
end)
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
	-- Create tags:
	for _,t in ipairs({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }) do
		awful.tag.add(t, {
			screen = s,
			layout = awful.layout.layouts[1],
			master_width_factor = is_laptop and 0.6 or 0.55,
		})
	end
	-- Focus on first tag:
	awful.screen.focused().tags[1]:view_only()

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
	s.mylayoutbox.forced_width = tonumber(last(gears.string.split(beautiful.font, " "))) + 4
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
		visible = true,
	})

	s.mywibox.widget = {
		layout = wibox.layout.align.horizontal,
		{
			spacing = 1,
			layout = wibox.layout.fixed.horizontal,
			s.mytaglist,
			{
				{ widget = s.mylayoutbox },
				widget = wibox.container.place,
			},
			s.mypromptbox,
		},
		s.mytasklist,
		{
			layout = wibox.layout.fixed.horizontal,
			spacing = 1,
			is_laptop and brightness_widget() or wibox.widget.base.empty_widget(),
			volume_widget(),
			is_laptop and battery_widget() or wibox.widget.base.empty_widget(),
			is_laptop and wibox.widget.base.empty_widget() or mpris_widget(),
			is_laptop and wibox.widget.base.empty_widget() or weather_widget({
				api_key=beautiful.ow.key,
				coordinates = beautiful.ow.coordinates,
				units = 'imperial',
				time_format_12h = true,
				both_units_widget = false,
				icons = 'VitalyGorbachev',
				icons_extension = '.svg',
				show_hourly_forecast = true,
				show_daily_forecast = true,
			}),
			is_laptop and wifi_widget() or wibox.widget.base.empty_widget(),
			clock_widget(),
			-- Add some space after the clock:
			wibox.widget.base.make_widget_declarative({
				forced_width = 6
			})
		}
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
	awful.key({ modkey, "Shift" }, "b", function()
		for s in screen do
			s.mywibox.visible = not s.mywibox.visible
		end
	end, {
		description = "toggle bar visibility",
		group = "awesome",
	})
})
-- Tag Manipulation {{{
awful.keyboard.append_global_keybindings({
	awful.key({ modkey, "Shift" }, "/", hotkeys_popup.show_help, { -- Mod4 + ?
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
	awful.key({ modkey, "Shift" }, "n", rofi.networkmanager, {
		description = "show network-manager menu",
		group = "launcher",
	}),
	awful.key({ modkey, "Control" }, "Spacebar", rofi.emoji, {
		description = "show emoji menu",
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
		brightness_widget:down()
	end, {
		description = "lower monitor brightness",
		group = "media",
	}),
	awful.key({}, "XF86MonBrightnessUp", function()
		brightness_widget:up()
	end, {
		description = "raise monitor brightness",
		group = "media",
	}),
	awful.key({}, "XF86KbdBrightnessDown", function()
		awful.spawn("sudo /usr/local/bin/keyboard-backlight down")
	end, {
		description = "lower keyboard backlight brightness",
		group = "media",
	}),
	awful.key({}, "XF86KbdBrightnessUp", function()
		awful.spawn("sudo /usr/local/bin/keyboard-backlight up")
	end, {
		description = "raise keyboard backlight brightness",
		group = "media",
	}),
	awful.key({}, "XF86AudioPlay", function() mpris_widget:play() end, {
		description = "play/pause audio",
		group = "media",
	}),
	awful.key({}, "XF86AudioStop", function() mpris_widget:stop() end, {
		description = "stop audio",
		group = "media",
	}),
	awful.key({}, "XF86AudioPrev", function() mpris_widget:previous() end, {
		description = "previous track",
		group = "media",
	}),
	awful.key({}, "XF86AudioNext", function() mpris_widget:next() end, {
		description = "next track",
		group = "media",
	}),
	awful.key({}, "XF86AudioLowerVolume", function() volume_widget:down() end, {
		description = "lower volume by 5%",
		group = "media",
	}),
	awful.key({}, "XF86AudioRaiseVolume", function() volume_widget:up() end, {
		description = "raise volume by 5%",
		group = "media",
	}),
	awful.key({}, "XF86AudioMute", function() volume_widget:mute() end, {
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
