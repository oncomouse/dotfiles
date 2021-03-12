-- ┏━┓╻ ╻┏━╸┏━┓┏━┓┏┳┓┏━╸╻ ╻┏┳┓
-- ┣━┫┃╻┃┣╸ ┗━┓┃ ┃┃┃┃┣╸ ┃╻┃┃┃┃
-- ╹ ╹┗┻┛┗━╸┗━┛┗━┛╹ ╹┗━╸┗┻┛╹ ╹
-- luacheck: globals awesome client root io

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
-- Make xresources colors global
x = {
	background = xrdb.background,
	foreground = xrdb.foreground,
	color0 = xrdb.color0,
	color1 = xrdb.color1,
	color2 = xrdb.color2,
	color3 = xrdb.color3,
	color4 = xrdb.color4,
	color5 = xrdb.color5,
	color6 = xrdb.color6,
	color7 = xrdb.color7,
	color8 = xrdb.color8,
	color9 = xrdb.color9,
	color10 = xrdb.color10,
	color11 = xrdb.color11,
	color12 = xrdb.color12,
	color13 = xrdb.color13,
	color14 = xrdb.color14,
	color15 = xrdb.color15,
}
-- Themes define colours, icons, font and wallpapers.
beautiful.init(gears.filesystem.get_themes_dir() .. "xresources/theme.lua")
beautiful.layout_centeredmonocle =
	gears.color.recolor_image(
		gears.filesystem.get_themes_dir() .. "default/layouts/maxw.png",
		beautiful.fg_normal
	)
beautiful.useless_gap = 0 -- No gaps
beautiful.border_normal = x.color8 -- Normal border color
beautiful.border_focus = x.color7 -- Focused border color
beautiful.font = "FiraCode Nerd Font Normal 15" -- Font
-- Widget spacing in left and right wibox areas:
beautiful.widget_space = {
	left = nil,
	right = " ⁞ ",
}
-- Hotkey formatting:
beautiful.hotkeys_modifiers_fg = x.color4
beautiful.hotkeys_font = "FiraCode Nerd Font Normal 16"
beautiful.hotkeys_description_font = "FiraCode Nerd Font Normal 12"
-- Titlebar formatting:
beautiful.titlebar_bg_focus = x.color7
-- Tasklist formatting:
beautiful.tasklist_disable_icon = true -- No icons in tasklist
-- Set the background:
gears.wallpaper.set(x.background)

-- This is used later as the default terminal and editor to run.
terminal = "kitty"

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.append_default_layouts{
	awful.layout.suit.tile,
	layout_cm,
	awful.layout.suit.max,
	awful.layout.suit.floating,
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
	awful.button({}, 1, swap_main),
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

awful.screen.connect_for_each_screen(function(s)
-- Store a list of verbs characters in a hash
-- Spawn in a terminal --luacheck: no unused args
-- Spawn with a shell --luacheck: no unused args
-- Quite dumb, don't do something like <num>+<adj>+<num>+<verb>
-- awful.prompt.run -- adjs, count, command -- adjs, count, command
	awful.tag(
		{ "1", "2", "3", "4", "5", "6", "7", "8", "9" },
		s,
		awful.layout.layouts[1]
	)
	local atextbox = wibox.widget.textbox()
	local verbs = {
		t = function(_, _, cmd)
			return { terminal, "-e", cmd }
		end,
		s = function(_, _, cmd)
			return { awful.util.shell, "-c", cmd }
		end,
	}
	local function vi_parse(action, command)
		local req, ret = {
			count = {},
			adjectives = {},
		}
		for char in action:gmatch("(.)") do
			if tonumber(char) then
				table.insert(req.count, char)
			elseif verbs[char] then
				req.verb = char
			else
				table.insert(ret.adjectives, char)
			end
			if req.verb then
				req.count = tonumber(table.concat(req.count)) or 1
				ret = ret or verbs[req.verb](req.adjectives, req.count, command)
				req = {
					count = {},
					adjectives = {},
				}
			end
		end
		return ret
	end
	s.mypromptbox = awful.widget.prompt{
		prompt = "<b>Run: </b>",
		hooks = { { {}, "Return", function(cmd)
			if not cmd or cmd:sub(1, 1) ~= ":" then
				return cmd
			end
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
	s.mytaglist = awful.widget.taglist{
		screen = s,
		filter = awful.widget.taglist.filter.all,
		buttons = taglist_buttons,
	}

	s.mytasklist = awful.widget.tasklist{
		screen = s,
		filter = awful.widget.tasklist.filter.focused,
		buttons = tasklist_buttons,
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
		position = "top",
		screen = s,
		height = "24",
	})

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
			clock_widget,
		},
	}
end)
-- }}}

-- ╻┏ ┏━╸╻ ╻   ┏┓ ╻┏┓╻╺┳┓╻┏┓╻┏━╸┏━┓
-- ┣┻┓┣╸ ┗┳┛   ┣┻┓┃┃┗┫ ┃┃┃┃┗┫┃╺┓┗━┓
-- ╹ ╹┗━╸ ╹    ┗━┛╹╹ ╹╺┻┛╹╹ ╹┗━┛┗━┛
-- {{{ Key bindings
globalkeys = gears.table.join(
	awful.key({ modkey }, "s", hotkeys_popup.show_help, {
		description = "show help",
		group = "awesome",
	}),
	awful.key(
		{ modkey, "Control" },
		"d",
		function()
			clock_widget.toggle()
		end,
		{
			description = "show/hide calendar",
			group = "awesome",
		}
	),
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
		{ modkey, "Control" },
		"m",
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
	), -- Standard program
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
		{ modkey },
		"i",
		function()
			awful.tag.incnmaster(1, nil, true)
		end,
		{
			description = "increase the number of master clients",
			group = "layout",
		}
	),
	awful.key(
		{ modkey },
		"d",
		function()
			awful.tag.incnmaster(-1, nil, true)
		end,
		{
			description = "decrease the number of master clients",
			group = "layout",
		}
	),
	awful.key(
		{ modkey, "Shift" },
		"i",
		function()
			awful.tag.incncol(1, nil, true)
		end,
		{
			description = "increase the number of columns",
			group = "layout",
		}
	),
	awful.key(
		{ modkey, "Shift" },
		"d",
		function()
			awful.tag.incncol(-1, nil, true)
		end,
		{
			description = "decrease the number of columns",
			group = "layout",
		}
	),
	-- awful.key(
	-- 	{ modkey },
	-- 	"space",
	-- 	function()
	-- 		awful.layout.inc(1)
	-- 	end,
	-- 	{
	-- 		description = "select next",
	-- 		group = "layout",
	-- 	}
	-- ),
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
			awful.layout.set(awful.layout.suit.tile.right)
		end,
		{
			description = "select tiled layout",
			group = "layout",
		}
	),
	awful.key(
		{ modkey, "Shift" },
		"t",
		function()
			awful.layout.set(awful.layout.suit.tile.left)
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
	), -- Prompt
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
				prompt = "<b>Run Lua code</b>: ",
				textbox = awful.screen.focused().mypromptbox.widget,
				exe_callback = function(s)
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
			group = "awesome",
		}
	), -- Menubar
	awful.key(
		{ modkey },
		"p",
		function()
			awful.spawn.with_shell("rofi -show drun -match fuzzy -show-icons")
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
	), -- Print Screen
	awful.key({}, "Print", function()
		awful.spawn.with_shell(
			"sleep 0.2 && " .. "scrot ~/'Seafile/My Library/Photos/Screenshots/%Y-%m-%d_%H%M%S-$wx$h_scrot.png'"
		)
	end),
	awful.key({ "Shift" }, "Print", function()
		awful.spawn.with_shell(
			"sleep 0.2 && " .. "scrot -s ~/'Seafile/My Library/Photos/Screenshots/%Y-%m-%d_%H%M%S-$wx$h_scrot.png'"
		)
	end), -- Media Keys
	awful.key({}, "XF86AudioPlay", function()
		mpris_widget:play()
	end),
	awful.key({}, "XF86AudioStop", function()
		mpris_widget:stop()
	end),
	awful.key({}, "XF86AudioPrev", function()
		mpris_widget:previous_track()
	end),
	awful.key({}, "XF86AudioNext", function()
		mpris_widget:next_track()
	end),
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
	awful.key({ modkey }, "c", function(c)
		if c.sticky then
			c.sticky = false
		else
			c.sticky = true
		end
	end),
	awful.key({ modkey, "Shift" }, "space", awful.client.floating.toggle, {
		description = "toggle floating",
		group = "client",
	}),
	awful.key({ modkey }, "Return", swap_main, {
		description = "move to master",
		group = "client",
	}),
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
		globalkeys, -- View tag only.
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
			keys = clientkeys,
			buttons = clientbuttons,
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
	local buttons = { awful.button({}, 1, function()
		c:activate{
			context = "titlebar",
			action = "mouse_move",
		}
	end), awful.button({}, 3, function()
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
			awful.titlebar.widget.floatingbutton(c),
			awful.titlebar.widget.maximizedbutton(c),
			awful.titlebar.widget.stickybutton(c),
			awful.titlebar.widget.ontopbutton(c),
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

-- vim: fdl=0:fdm=marker
