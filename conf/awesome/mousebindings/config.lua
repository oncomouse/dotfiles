local beautiful = require("beautiful")
local awful = require("awful")
-- Mouse behavior for layoutbox:
beautiful.layoutbox_mousebuttons = {
	awful.button({}, awful.button.names.LEFT, function()
		awful.layout.inc(1)
	end),
	awful.button({}, awful.button.names.RIGHT, function()
		-- awful.layout.inc(-1)
		require("layouts.menu")()
	end),
	awful.button({}, awful.button.names.SCROLL_UP, function()
		awful.layout.inc(1)
	end),
	awful.button({}, awful.button.names.SCROLL_DOWN, function()
		awful.layout.inc(-1)
	end),
}
-- Mouse behavior for taglist:
beautiful.taglist_mousebuttons = {
	awful.button({}, awful.button.names.LEFT, function(t)
		t:view_only()
	end),
	awful.button({ beautiful.modkey }, 1, function(t)
		if client.focus then
			client.focus:move_to_tag(t)
		end
	end),
	awful.button({}, awful.button.names.RIGHT, awful.tag.viewtoggle),
	awful.button({ beautiful.modkey }, 3, function(t)
		if client.focus then
			client.focus:toggle_tag(t)
		end
	end),
	awful.button({}, awful.button.names.SCROLL_UP, function(t)
		awful.tag.viewprev(t.screen)
	end),
	awful.button({}, awful.button.names.SCROLL_DOWN, function(t)
		awful.tag.viewnext(t.screen)
	end),
}
-- Mouse behavior for tasklist:
beautiful.tasklist_mousebuttons = {
	awful.button({}, awful.button.names.LEFT, require("utils.swap_main")),
	awful.button({}, awful.button.names.RIGHT, function()
		awful.menu.client_list({ theme = { width = 250 } })
	end),
	awful.button({}, awful.button.names.SCROLL_UP, function()
		awful.client.focus.byidx(1)
	end),
	awful.button({}, awful.button.names.SCROLL_DOWN, function()
		awful.client.focus.byidx(-1)
	end),
}
beautiful.client_mousebuttons = {
	awful.button({}, awful.button.names.LEFT, function(c)
		c:activate({ context = "mouse_click" })
	end),
	awful.button({ beautiful.modkey }, 1, function(c)
		c:activate({ context = "mouse_click", action = "mouse_move" })
	end),
	awful.button({ beautiful.modkey }, 3, function(c)
		c:activate({ context = "mouse_click", action = "mouse_resize" })
	end),
}
