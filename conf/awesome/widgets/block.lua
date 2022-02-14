-- luacheck: globals awesome
local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local wibox = require("wibox")

-- local installed_penlight,path = pcall(require, "pl.path")
-- require("naughty").notification({ title = path.dirname(path.abspath(debug.getinfo(1).short_src)) })

local Block = function(def)
	local Widget = {}
	function Widget:init(opts)
		def = gears.table.crush(def, opts)
		-- Create the widget:
		self.widget = wibox.widget({
			{
				{
					-- Extend the textbox with an additional options passed:
					def.widget or gears.table.join({
						id = "output",
						markup = "",
						widget = wibox.widget.textbox,
					}, def.widget_options or {}),
					widget = wibox.container.margin,
					margins = def.margins or beautiful.block_margins or {
						left = 5,
						right = 5,
					},
				},
				widget = wibox.container.background,
				bg = def.bg or beautiful.tasklist_bg_focus,
				fg = def.fg or beautiful.tasklist_fg_focus,
			},
			layout = wibox.layout.fixed.horizontal,
			-- The update function, which changes markup:
			update = function(output)
				if self.widget.children[1].children[1].output then
					self.widget.children[1].children[1].output.markup = output
				end
			end,
		})

		-- Update calls the widget's update, but it also has an attached state:
		self.state = {}
		self.update = setmetatable({}, {
			__call = function(_, output)
				self.widget.update(output)
			end,
			__index = function(_, index)
				return index == "state" and self.state or nil
			end,
		})

		-- Handle callback functions:
		if type(def.callback) == "function" then
			self.request_update = function()
				def.callback(self.update)
			end
			-- And signal names:
		elseif type(def.callback) == "string" then
			self.request_update = function()
				awesome.emit_signal(def.callback)
			end
			-- And no callbacks:
		else
			self.request_update = function()
				if self.widget.update then
					self.widget.update("")
				end
			end
		end

		-- Attach button listeners:
		-- def.buttons is a table with button number = function(<update>)
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
				awful.button({}, awful.button.names.LEFT, function()
					self.request_update()
				end),
			}
		end

		-- Create a timer if this block is periodically calling something. I mostly
		-- do this in the signal definitions themselves, but it's still here:
		if type(def.timeout) == "number" and def.timeout > 0 then
			self.timer = gears.timer({
				timeout = def.timeout,
				call_now = true,
				autostart = true,
				callback = self.request_update,
			})
		end

		-- Attach any signal listeners:
		-- def.signals is table with signal_name = function(update, ...) where ...
		-- is the args passed by the signal itself.
		if type(def.signals) == "table" then
			for signal, cb in pairs(def.signals) do
				awesome.connect_signal(signal, function(...)
					local args = { ... }
					cb(table.unpack(gears.table.join({ self.update }, args)))
				end)
			end
		end
		-- Trigger an update to get initial content:
		self.request_update()
		return self
	end
	return setmetatable(Widget, { __call = Widget.init })
end

return Block
