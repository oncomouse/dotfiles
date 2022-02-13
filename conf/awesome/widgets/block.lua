-- luacheck: globals awesome
local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")

local Block = function(def)
	local Widget = {}
	function Widget:new()
		return setmetatable({}, { __index = self }):init()
	end
	function Widget:init()
		self.widget = def.widget
			or wibox.widget({
				{
					gears.table.join({
						id = "output",
						markup = "",
						widget = wibox.widget.textbox,
					}, def.widget_options or {}),
					widget = wibox.container.background,
					bg = def.bg or beautiful.tasklist_bg_focus,
					fg = def.fg or beautiful.tasklist_fg_focus,
				},
				layout = wibox.layout.fixed.horizontal,
				update = function(output)
					self.widget.children[1].output.markup = output
				end,
			})

		local state = {}
		self.update = setmetatable({}, {
			__call = function(_, output)
				self.widget.update(output)
			end,
			__index = function(_, index)
				return index == "state" and state or nil
			end
		})

		if type(def.callback) == "function" then
			self.request_update = function()
				def.callback(self.update)
			end
		elseif type(def.callback) == "string" then
			self.request_update = function()
				awesome.emit_signal(def.callback)
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
						button(self.update)
					end)
				)
			end
			self.widget.buttons = buttons
		else
			self.widget.buttons = {
				awful.button({}, awful.button.names.LEFT, function() self.request_update() end)
			}
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
					cb(table.unpack(gears.table.join({ self.update }, args)))
				end)
			end
		end
		self.request_update()
		return self
	end
	return setmetatable(Widget, { __call = Widget.new })
end

return Block
