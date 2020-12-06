function control_ncspot(method)
	return function()
		local command =
			"/usr/bin/env dbus-send --print-reply --dest=org.mpris.MediaPlayer2.ncspot /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player." .. method
		hs.execute(command, true)
	end
end

return control_ncspot
