local wibox = require("wibox")

local function make_wibar_widgets(widget_definitions)
	local widgets = wibox.widget({
		layout = wibox.layout.fixed.horizontal,
		widget = wibox.container.place,
	})

	for _, widget in ipairs(widget_definitions) do
		local name = type(widget) == "string" and widget or widget.name
		local opts = type(widget) == "string" and {} or widget.opts
		local ok, widget_def = pcall(require, "widgets." .. name)
		if ok then
			local w = widget_def(opts).widget
			table.insert(widgets.children, w)
		end
	end
	return widgets
end

return make_wibar_widgets
