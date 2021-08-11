local mpris = require("evil.mpris")
local wifi = require("evil.wifi")
local battery = require("evil.battery")
local brightness = require("evil.brightness")
local is_laptop = require("utils.is_laptop")

return function()
	if is_laptop() then
		wifi()
		battery()
		brightness()
	else
		mpris()
	end
end
