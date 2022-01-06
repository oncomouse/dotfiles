-- luacheck: globals awesome
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local widget_signals = {}
local function block_watcher(cmd, delay, name)
	local widget = awful.widget.watch(cmd, delay)
	local bg_container = wibox.container.background()
	bg_container:set_widget(widget)
	bg_container:set_bg(beautiful.tasklist_bg_focus)
	bg_container:set_fg(beautiful.tasklist_fg_focus)
	-- Trigger for button presses
	widget:connect_signal("button::press", function(_, _, _, button)
		awful.spawn.easy_async_with_shell("env BUTTON=" .. button .. " " .. cmd, function()
			widget:emit_signal("widget::update")
		end)
	end)
	if name then
		widget_signals[name] = widget
		-- Internal keypress handler:
		widget:connect_signal("widget::update", function()
			awful.spawn.easy_async_with_shell(cmd, function(stdout)
				widget:set_text(stdout)
			end)
		end)
	end
	return bg_container
end

-- System-wide signal dispersal for key presses
awesome.connect_signal("widget::update", function(name)
	if widget_signals[name] then
		local widget = widget_signals[name]
		widget:emit_signal("widget::update")
	end
end)

local function make_wibar_widgets(widget_definitions)
	local widgets = wibox.widget({
		layout = wibox.layout.fixed.horizontal,
		spacing = 10,
		spacing_widget = {
			text = " ",
			widget = wibox.widget.textbox,
		},
		widget = wibox.container.place,
	})

	for _, widget in ipairs(widget_definitions) do
		table.insert(widgets.children, block_watcher(widget[1], widget[2], widget[3]))
	end
	return widgets
end

return make_wibar_widgets
