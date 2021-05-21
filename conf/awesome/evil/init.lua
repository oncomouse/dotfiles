local mpris = require("evil.mpris")
local volume = require("evil.volume")
local wifi = require("evil.wifi")
local battery = require("evil.battery")
local brightness = require("evil.brightness")
local is_laptop = require("utils.is_laptop")

return function()
	volume()
	wifi()
	if is_laptop() then
		battery()
		brightness()
	else
		mpris()
	end
end
