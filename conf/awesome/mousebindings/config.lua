-- luacheck: globals awesome screen client
local beautiful = require("beautiful")
local awful = require("awful")
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

