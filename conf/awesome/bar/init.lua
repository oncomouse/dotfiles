local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
local is_laptop = require("utils.is_laptop")
screen.connect_signal("request::desktop_decoration", function(s)
	-- s.systray = wibox.widget.systray()
	if is_laptop then
		s.layoutbox = wibox.widget.textbox("")
	else
		s.layoutbox = awful.widget.layoutbox(s)
		s.layoutbox:buttons(beautiful.layoutbox_mousebuttons)
		s.layoutbox = wibox.container.margin(s.layoutbox, 4, 4, 4, 4)
	end
	s.taglist = awful.widget.taglist({
		screen = s,
		filter = awful.widget.taglist.filter.noempty,
		buttons = beautiful.taglist_mousebuttons,
	})
	s.tasklist = awful.widget.tasklist({
		screen = s,
		filter = awful.widget.tasklist.filter.focused,
		buttons = beautiful.tasklist_mousebuttons,
		widget_template = {
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
		},
	})

	s.wibar = awful.wibar({
		position = beautiful.bar_position,
		screen = s,
		height = beautiful.bar_height,
		visible = true,
	})
	s.widget_bar = require("widgets")
	s.wibar.widget = {
		layout = wibox.layout.align.horizontal,
		-- Left Bar:
		{
			spacing = 1,
			layout = wibox.layout.fixed.horizontal,
			s.taglist,
			{
				{ widget = s.layoutbox },
				widget = wibox.container.place,
			},
		},
		-- Center Bar:
		is_laptop and wibox.widget.textbox or s.tasklist,
		-- Right Bar:
		{
			-- {
			-- 	{ s.systray, layout = wibox.layout.fixed.horizontal },
			-- 	right = 10,
			-- 	top = 2,
			-- 	bottom = 2,
			-- 	widget = wibox.container.margin,
			-- },
			s.widget_bar,
			spacing = 0,
			layout = wibox.layout.fixed.horizontal,
		},
	}
end)
