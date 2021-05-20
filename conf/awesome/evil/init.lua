local mpris = require("evil.mpris")
local volume = require("evil.volume")

return function()
	volume()
	mpris()
end
