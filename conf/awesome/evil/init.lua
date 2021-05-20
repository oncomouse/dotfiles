local mpris = require("evil.mpris")
local volume = require("evil.volume")
local wifi = require("evil.wifi")

return function()
	volume()
	mpris()
	wifi()
end
