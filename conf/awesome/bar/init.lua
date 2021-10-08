-- luacheck: globals screen awesome
local awful = require("awful")
local beautiful = require("beautiful")
local wibox = require("wibox")
require("widgets")
screen.connect_signal("request::desktop_decoration", function(s)
	for _, t in ipairs(beautiful.tags) do
		awful.tag.add(t, {
			screen = s,
			layout = awful.layout.layouts[1],
			master_width_factor = beautiful.mfact,
		})
	end
	awful.screen.focused().tags[1]:view_only()
	s.layoutbox = awful.widget.layoutbox(s)
	s.layoutbox:buttons(beautiful.layoutbox_mousebuttons)
	s.layoutbox = wibox.container.margin(s.layoutbox, 4, 4, 4, 4)
	-- s.layoutbox.forced_width = tonumber(last(gears.string.split(beautiful.font, " "))) + 4
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
	s.wibar.widget = {
		layout = wibox.layout.align.horizontal,
		{
			spacing = 1,
			layout = wibox.layout.fixed.horizontal,
			s.taglist,
			{
				{ widget = s.layoutbox },
				widget = wibox.container.place,
			},
		},
		s.tasklist,
		require("widgets.make").make(beautiful.wibar_widgets),
	}
end)
