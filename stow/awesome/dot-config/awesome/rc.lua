-- ┏━┓╻ ╻┏━╸┏━┓┏━┓┏┳┓┏━╸╻ ╻┏┳┓
-- ┣━┫┃╻┃┣╸ ┗━┓┃ ┃┃┃┃┣╸ ┃╻┃┃┃┃
-- ╹ ╹┗┻┛┗━╸┗━┛┗━┛╹ ╹┗━╸┗┻┛╹ ╹
-- luacheck: globals awesome client io

-- ┏━┓┏━┓╺┳╸╻ ╻
-- ┣━┛┣━┫ ┃ ┣━┫
-- ╹  ╹ ╹ ╹ ╹ ╹
-- Path {{{
-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Update package.path
local addtional_path_prefix = os.getenv("HOME") .. "/dotfiles/conf/awesome/"
local addtional_path =
	";" .. addtional_path_prefix .. "?/init.lua;" .. addtional_path_prefix .. "?.lua"
package.path = package.path .. addtional_path

-- Set log file:
local logfile = io.open("/tmp/myawesome.log", "a")
logfile:write("\n\n\nLoading Awesome\nPath: ", package.path, "\n\n")
io.stderr = logfile
-- }}}

-- ╻┏┓╻┏━╸╻  ╻ ╻╺┳┓┏━╸┏━┓
-- ┃┃┗┫┃  ┃  ┃ ┃ ┃┃┣╸ ┗━┓
-- ╹╹ ╹┗━╸┗━╸┗━┛╺┻┛┗━╸┗━┛
-- Includes {{{
-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Rules
local ruled = require("ruled")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
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
-- }}}

-- ┏━╸┏━┓┏━┓┏━┓┏━┓   ╻ ╻┏━┓┏┓╻╺┳┓╻  ╻┏┓╻┏━╸
-- ┣╸ ┣┳┛┣┳┛┃ ┃┣┳┛   ┣━┫┣━┫┃┗┫ ┃┃┃  ┃┃┗┫┃╺┓
-- ┗━╸╹┗╸╹┗╸┗━┛╹┗╸   ╹ ╹╹ ╹╹ ╹╺┻┛┗━╸╹╹ ╹┗━┛
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

-- ┏━┓┏━┓┏━┓┏━╸┏━┓┏━┓┏━┓┏┓╻┏━╸┏━╸
-- ┣━┫┣━┛┣━┛┣╸ ┣━┫┣┳┛┣━┫┃┗┫┃  ┣╸
-- ╹ ╹╹  ╹  ┗━╸╹ ╹╹┗╸╹ ╹╹ ╹┗━╸┗━╸
-- {{{ Appearance
-- Load xrdb colors:
local xrdb = beautiful.xresources.get_current_theme()

-- Themes define colours, icons, font and wallpapers. {{{
beautiful.init(gears.filesystem.get_themes_dir() .. "xresources/theme.lua")
beautiful.layout_centeredmonocle = gears.color.recolor_image(
	gears.filesystem.get_themes_dir() .. "default/layouts/magnifierw.png",
	beautiful.fg_normal
)
beautiful.useless_gap = 0 -- No gaps
beautiful.border_normal = xrdb.color8 -- Normal border color
beautiful.border_focus = xrdb.color7 -- Focused border color
beautiful.font = "FiraCode Nerd Font Normal 15" -- Font
-- Widget spacing in left and right wibox areas:
beautiful.widget_space = {
	left = nil,
	right = " ⁞ ",
}
-- Wibar stuff:
beautiful.bar_height = 24
beautiful.bar_position = "top"
-- Hotkey formatting:
beautiful.hotkeys_modifiers_fg = xrdb.color4
beautiful.hotkeys_font = "FiraCode Nerd Font Normal 16"
beautiful.hotkeys_description_font = "FiraCode Nerd Font Normal 12"
-- Titlebar formatting:
beautiful.titlebar_bg_focus = xrdb.color7
-- Tasklist formatting:
beautiful.tasklist_disable_icon = true -- No icons in tasklist
--- }}}

-- Set the background:
gears.wallpaper.set(xrdb.background)
-- This is used later as the default terminal and editor to run.
terminal = "kitty"
-- Default modkey.
modkey = "Mod4"
-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.append_default_layouts{
	awful.layout.suit.tile.right,
	awful.layout.suit.tile.left,
	layout_cm,
}
-- }}}

-- ╻ ╻╻┏┓ ┏━┓┏━┓
-- ┃╻┃┃┣┻┓┣━┫┣┳┛
-- ┗┻┛╹┗━┛╹ ╹╹┗╸
-- {{{ Wibar
-- Mpris Player Status Widget:
local mpris_widget = require("widgets.mpris")
-- Volume Widget:
local volume_widget = require("widgets.volume")
-- Clock Widget:
local clock_widget = require("widgets.clock")
awful.screen.connect_for_each_screen(function(s)
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
			clock_widget,
		},
	}
end)
-- }}}

-- ╻┏ ┏━╸╻ ╻   ┏┓ ╻┏┓╻╺┳┓╻┏┓╻┏━╸┏━┓
-- ┣┻┓┣╸ ┗┳┛   ┣┻┓┃┃┗┫ ┃┃┃┃┗┫┃╺┓┗━┓
-- ╹ ╹┗━╸ ╹    ┗━┛╹╹ ╹╺┻┛╹╹ ╹┗━┛┗━┛
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
			"sleep 0.2 && " .. "scrot ~/'Seafile/My Library/Photos/Screenshots/%Y-%m-%d_%H%M%S-$wx$h_scrot.png'"
		)
	end, {
		description = "take a screenshot",
		group = "media",
	}),
	awful.key({ "Shift" }, "Print", function()
		awful.spawn.with_shell(
			"sleep 0.2 && " .. "scrot -s ~/'Seafile/My Library/Photos/Screenshots/%Y-%m-%d_%H%M%S-$wx$h_scrot.png'"
		)
	end, {
		description = "take a screenshot (select region)",
		group = "media",
	})
})
--- }}}
-- Media Keys {{{
awful.keyboard.append_global_keybindings({
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
	})
end)
-- }}}
-- }}}

-- ┏━┓╻ ╻╻  ┏━╸┏━┓
-- ┣┳┛┃ ┃┃  ┣╸ ┗━┓
-- ╹┗╸┗━┛┗━╸┗━╸┗━┛
-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
ruled.client.connect_signal("request::rules", function()
	ruled.client.append_rule{
		-- All clients will match this rule.
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
	ruled.client.append_rule{
		rule_any = {
			class = { "feh", "Gimp", "Thunar", "files", "Files", "Pcmanfm" },
		},
		properties = {
			floating = true,
			titlebars_enabled = true,
		},
	}
	ruled.client.append_rule{
		rule_any = {
			class = { "libreoffice" },
		},
		properties = { titlebars_enabled = true },
	}
	ruled.client.append_rule{
		rule_any = {
			class = { "Thunar", "files", "Files", "Pcmanfm" },
		},
		properties = {
			placement = awful.placement.centered,
			width = 1024,
			height = 615,
		},
	}
	ruled.client.append_rule{
		-- Add titlebars to normal clients and dialogs
		rule_any = {
			type = { "dialog" },
		},
		properties = { titlebars_enabled = true },
	}
	ruled.client.append_rule{
		rule = { class = "zoom" },
		properties = {
			screen = 1,
			tag = "7",
		},
	}
	ruled.client.append_rule{
		rule = { class = "Zotero" },
		properties = {
			screen = 1,
			tag = "8",
		},
	}
end)
client.connect_signal("request::titlebars", function(c)
	-- buttons for the titlebar
	local buttons = {
		awful.button({}, 1, function() c:activate{
			context = "titlebar",
			action = "mouse_move",
		}
	end),
	awful.button({modkey}, 3, function() rofi.client_flags(c) end),
	awful.button({}, 3, function()
		c:activate{
			context = "titlebar",
			action = "mouse_resize",
		}
	end) }

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
				font = "FiraCode Nerd Font Bold 12",
				widget = awful.titlebar.widget.titlewidget(c),
			},
			buttons = buttons,
			layout = wibox.layout.flex.horizontal,
		},
		{
			-- Right
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

-- ┏━┓╻┏━╸┏┓╻┏━┓╻  ┏━┓
-- ┗━┓┃┃╺┓┃┗┫┣━┫┃  ┗━┓
-- ┗━┛╹┗━┛╹ ╹╹ ╹┗━╸┗━┛
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
-- }}}

-- vim: foldlevel=0:foldmethod=marker
