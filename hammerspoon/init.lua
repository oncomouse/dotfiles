--luacheck: globals hs
hyper = { "ctrl", "alt", "cmd", "shift" }
-- Capslock + space -> console
hs.hotkey.bindSpec({ hyper, "space" }, hs.toggleConsole)

-- window management:
hs.window.animationDuration = 0
hs.hotkey.bindSpec({ hyper, "left" }, function()
	local win = hs.window.focusedWindow()
	if not win then return end
	win:moveToUnit(hs.layout.left50)
end)
hs.hotkey.bindSpec({ hyper, "up" }, function()
	local win = hs.window.focusedWindow()
	if not win then return end
	win:moveToUnit(hs.layout.maximized)
end)
hs.hotkey.bindSpec({ hyper, "right" }, function()
	local win = hs.window.focusedWindow()
	if not win then return end
	win:moveToUnit(hs.layout.right50)
end)
-- Create a nice centered reading window:
hs.hotkey.bindSpec({ hyper, "down" }, function()
	local win_percentage = .95
	local offset = (1 - win_percentage) / 2
	local win = hs.window.focusedWindow()
	if not win then return end
	win:moveToUnit(
		hs.geometry.rect(offset, offset, win_percentage, win_percentage),
		0
	)
end)
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
hs.hotkey.bind(hyper, "0", function()
	hs.reload()
end)
configWatcher =
	hs.pathwatcher.new(
		os.getenv("HOME") .. "/.hammerspoon/",
		reloadConfig
	):start()
hs.alert.show("Config loaded")
