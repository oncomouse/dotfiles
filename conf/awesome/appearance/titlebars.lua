-- luacheck: globals client tag
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
client.connect_signal("request::titlebars", function(c)
	-- buttons for the titlebar
	local buttons = {
		awful.button({}, 1, function()
			c:activate({ context = "titlebar", action = "mouse_move" })
		end),
		awful.button({}, 3, function()
			c:activate({ context = "titlebar", action = "mouse_resize" })
		end),
	}

	awful.titlebar(c, {
		font = beautiful.titlebar_font or beautiful.font,
	}).widget = {
		{ -- Left
			awful.titlebar.widget.iconwidget(c),
			buttons = buttons,
			layout = wibox.layout.fixed.horizontal,
		},
		{ -- Middle
			{ -- Title
				align = "center",
				widget = awful.titlebar.widget.titlewidget(c),
			},
			buttons = buttons,
			layout = wibox.layout.flex.horizontal,
		},
		{ -- Right
			awful.titlebar.widget.maximizedbutton(c),
			awful.titlebar.widget.stickybutton(c),
			awful.titlebar.widget.ontopbutton(c),
			awful.titlebar.widget.closebutton(c),
			layout = wibox.layout.fixed.horizontal(),
		},
		layout = wibox.layout.align.horizontal,
	}
end)

local function set_titlebar(client, s)
	if not client.can_titlebar then
		return
	end
	if s then
		if client.titlebar == nil then
			client:emit_signal("request::titlebars", "rules", {})
		end
		awful.titlebar.show(client)
	else
		awful.titlebar.hide(client)
	end
end

local function truefloat(c)
	return c.floating and not c.maximized and not c.fullscreen
end

-- Hook called when a client spawns
client.connect_signal("manage", function(c)
	set_titlebar(c, truefloat(c) or c.first_tag.layout == awful.layout.suit.floating)
end)

tag.connect_signal("property::layout", function(t)
	for _, c in pairs(t:clients()) do
		if t.layout == awful.layout.suit.floating then
			set_titlebar(c, true)
		else
			set_titlebar(c, false)
		end
	end
end)

client.connect_signal("property::floating", function(c)
	set_titlebar(c, truefloat(c) or c.first_tag and c.first_tag.layout.name == "floating")
end)
