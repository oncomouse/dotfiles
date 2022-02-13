-- luacheck: globals awesome
local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")

local Block = function(def)
	local Widget = {}
	function Widget:new()
		return setmetatable({}, { __index = self }):init()
	end
	function Widget:init()
		self.widget = def.widget or wibox.widget({
			gears.table.join({
				id = "output",
				markup = "",
				widget = wibox.widget.textbox,
			}, def.widget_options or {}),
			layout = wibox.layout.fixed.horizontal,
			update = function(output)
				self.widget.output.markup = output
			end,
		})
		if type(def.cb) == "function" then
			self.request_update = function()
				def.cb(self.widget.update)
			end
		elseif type(def.cb) == "string" then
			self.request_update = function()
				awesome.emit_signal(def.cb)
			end
		else
			self.request_update = function()
				if self.widget.update then
					self.widget.update("")
				end
			end
		end
		if type(def.buttons) == "table" then
			local buttons = {}
			for num, button in pairs(def.buttons) do
				table.insert(
					buttons,
					awful.button({}, tonumber(num), function()
						button(self.widget.update)
					end)
				)
			end
			self.widget.buttons = buttons
		end
		if type(def.timeout) == "number" and def.timeout > 0 then
			self.timer = gears.timer({
				timeout = def.timeout,
				call_now = true,
				autostart = true,
				callback = self.request_update,
			})
		end
		if type(def.signals) == "table" then
			for signal, cb in pairs(def.signals) do
				awesome.connect_signal(signal, function(...)
					local args = { ... }
					cb(table.unpack(gears.table.join({ self.widget.update }, args)))
				end)
			end
		end
		self.request_update()
		return self
	end
	return setmetatable(Widget, { __call = Widget.new })
end

return Block
