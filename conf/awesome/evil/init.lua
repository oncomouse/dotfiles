require("evil.volume")
require("evil.mpd")
if require("utils.is_laptop") then
	require("evil.battery")
	require("evil.brightness")
end
