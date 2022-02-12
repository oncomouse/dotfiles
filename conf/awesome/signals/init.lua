require("signals.volume")
-- require("signals.mpd")
if require("utils.is_laptop") then
	require("signals.battery")
	require("signals.brightness")
else
	require("signals.mpris")
end
