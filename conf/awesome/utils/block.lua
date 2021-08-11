--luacheck: globals awesome
local awful = require("awful")
local blocks = 1
local function block(command, timeout)
	local widget,_ = awful.widget.watch(command, timeout, function(widget, stdout)
		widget:set_markup(stdout)
	end)
	local function trigger_click(button)
		awful.spawn.easy_async_with_shell("env BUTTON='"..button.."' " ..command, function(stdout)
			widget:set_markup(stdout)
		end)
	end
	widget:add_button(awful.button({
		modifiers = {},
		button = awful.button.names.LEFT,
		on_release = function() trigger_click(1) end
	}))
	widget:add_button(awful.button({
		modifiers = {},
		button = awful.button.names.MIDDLE,
		on_release = function() trigger_click(2) end
	}))
	widget:add_button(awful.button({
		modifiers = {},
		button = awful.button.names.RIGHT,
		on_release = function() trigger_click(3) end
	}))
	widget:add_button(awful.button({
		modifiers = {},
		button = awful.button.names.SCROLL_DOWN,
		on_release = function() trigger_click(4) end
	}))
	widget:add_button(awful.button({
		modifiers = {},
		button = awful.button.names.SCROLL_UP,
		on_release = function() trigger_click(5) end
	}))
	local signal_name = blocks
	blocks = blocks + 1
	local signal = "dotfiles::block::" .. signal_name
	awesome.connect_signal(signal, function()
		awful.spawn.easy_async_with_shell(command, function(stdout)
			widget:set_markup(stdout)
		end)
	end)
	return widget, signal
end

return block
