--luacheck: globals awesome
local awful = require("awful")
local gears = require("gears")
local wibox = require("wibox")
local blocks = 1

function block_watch(command, timeout, callback, base_widget)
	timeout = timeout or 5
	base_widget = base_widget or wibox.widget.textbox()
	callback = callback or function(widget, stdout, stderr, exitreason, exitcode) -- luacheck: no unused args
		widget:set_text(stdout)
	end
	local t = timer { timeout = timeout }
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

local function block(command, timeout)
	-- Update:
	local function update(widget, output)
		widget:set_markup(output)
	end
	-- Signal:
	local signal_name = blocks
	blocks = blocks + 1
	local signal = "dotfiles::block::" .. signal_name
	-- Widget and Click Handler:
	local widget,_ = block_watch(command, timeout, function(widget, stdout)
		update(widget, stdout)
	end)
	-- Attach click handlers for each button:
	for i,key in ipairs(gears.table.keys(awful.button.names)) do
		widget:add_button(awful.button({
			modifiers = {},
			button = awful.button.names[key],
			on_press = function() 
				awful.spawn.easy_async_with_shell("env BUTTON='"..i.."' " ..command, function(stdout)
					update(widget, stdout)
				end)
			end
		}))
	end
	-- Connect the control signal for internal updates:
	awesome.connect_signal(signal, function()
		awful.spawn.easy_async_with_shell(command, function(stdout)
			update(widget, stdout)
		end)
	end)
	local function trigger(...)
		local arg = {...}
		local callback = table.remove(arg)
		require("naughty").notify({ text = command .. " " .. table.concat(arg, " ") })
		awful.spawn.easy_async_with_shell(command .. " " .. table.concat(arg, " "), callback)
	end
	return widget, signal, trigger
end

return block
