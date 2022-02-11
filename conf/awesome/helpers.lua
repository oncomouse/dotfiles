local gears = require("gears")
local wibox = require("wibox")
local helpers = {}
helpers.colorize_text = function(text, color)
	return "<span foreground='"..color.."'>"..text.."</span>"
end
helpers.rrect = function(radius)
    return function(cr, width, height)
        gears.shape.rounded_rect(cr, width, height, radius)
    end
end
helpers.vertical_pad = function(height)
    return wibox.widget{
        forced_height = height,
        layout = wibox.layout.fixed.vertical
    }
end
return helpers
