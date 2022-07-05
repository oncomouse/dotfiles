local brightness = 100
widget = luastatus.require_plugin("backlight-linux").widget({
	cb = function(br)
		if br ~= nil and br ~= brightness then
			brightness = br * 100
		end
		return string.format("[%s %0.0f%%]", "", brightness)
	end
})
