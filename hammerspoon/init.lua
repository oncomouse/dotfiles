hyper = { "ctrl", "alt", "cmd", "shift" }
-- Capslock + space -> console
hs.hotkey.bindSpec({ hyper, "space" }, hs.toggleConsole)

-- Auto-reload configuration:
function reloadConfig(files)
	doReload = false
	for _, file in pairs(files) do
		if file:sub(-4) == ".lua" then
			doReload = true
		end
	end
	if doReload then
		hs.reload()
	end
end
configWatcher =
	hs.pathwatcher.new(
		os.getenv("HOME") .. "/.hammerspoon/",
		reloadConfig
	):start()
hs.alert.show("Config loaded")
