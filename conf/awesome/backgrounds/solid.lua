local gears = require("gears")
local beautiful = require("beautiful")
local xrdb = beautiful.xresources.get_current_theme()

return function()
	gears.wallpaper.set(xrdb.background)
end
