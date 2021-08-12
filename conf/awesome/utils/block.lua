--luacheck: globals awesome
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local blocks = 1

local function watch(command, timeout, callback, base_widget)
	timeout = timeout or 5
	base_widget = base_widget or wibox.widget.textbox()
	callback = callback or function(widget, stdout, stderr, exitreason, exitcode) -- luacheck: no unused args
		widget:set_text(stdout)
	end
	local t = gears.timer { timeout = timeout }
	t:connect_signal("timeout", function()
		t:stop()
		awful.spawn.easy_async_with_shell(command, function(stdout, stderr, exitreason, exitcode)
			callback(base_widget, stdout, stderr, exitreason, exitcode)
			t:again()
		end)
	end)
	t:start()
	t:emit_signal("timeout")
	return base_widget, t
end

-- Helper Functions:
local function update(widget, output)
	widget:set_markup(output)
end
local function run(command, widget)
	awful.spawn.easy_async_with_shell(command, function(stdout)
		update(widget, stdout)
	end)
end

local function block(command, timeout)
	-- Signal:
	local signal_name = blocks
	blocks = blocks + 1
	local signal = "dotfiles::block::" .. signal_name
	-- Widget and Click Handler:
	local widget = watch(command, timeout, update)
	-- Attach click handlers for each button:
	for i,key in ipairs(gears.table.keys(awful.button.names)) do
		widget:add_button(awful.button({
			modifiers = {},
			button = awful.button.names[key],
			on_press = function()
				run("env BUTTON='"..i.."' " ..command, widget)
			end
		}))
	end
	-- Connect the control signal for internal updates:
	awesome.connect_signal(signal, function()
		run(command, widget)
	end)
	return widget, signal
end

return block
