local beautiful = require("beautiful")
local gears = require("gears")
function rc(names, template_func)
	local icons = {}
	for _,name in ipairs(names) do
		icons[name] = gears.color.recolor_image(beautiful.icon_dir .. "/" .. template_func(name), beautiful.fg_icon)
	end
	return icons
end
return rc
