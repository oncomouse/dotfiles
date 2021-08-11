--luacheck: globals awesome
local awful = require("awful")
local gears = require("gears")
local blocks = 1
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
	local widget,_ = awful.widget.watch(command, timeout, function(widget, stdout)
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
	return widget, signal
end

return block
