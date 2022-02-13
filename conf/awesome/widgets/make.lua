-- luacheck: globals awesome
local wibox = require("wibox")

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
		local ok, widget_def = pcall(require, "widgets." .. widget)
		if ok then
			local w = widget_def({}).widget
			table.insert(widgets.children, w)
		end
	end
	return widgets
end

return make_wibar_widgets
