require("signals.volume")
if require("utils.is_laptop") then
	require("signals.battery")
	require("signals.brightness")
	require("signals.mpd")
else
	require("signals.mpris")
	require("signals.heartbeat")
end
