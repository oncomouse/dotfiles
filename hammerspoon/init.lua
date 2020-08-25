hyper = { "ctrl", "alt", "cmd", "shift" }
function move_window(direction)
	return function()
		local win = hs.window.focusedWindow()
		local app = win:application()
		local app_name = app:name()
		local f = win:frame()
		local screen = win:screen()
		local max = screen:frame()
		if direction == "left" then
			f.x = max.x + 6
			f.w = (max.w / 2) - 9
		elseif direction == "right" then
			f.x = (max.x + (max.w / 2)) + 3
			f.w = (max.w / 2) - 9
		elseif direction == "up" then
			f.x = max.x + 6
			f.w = max.w - 12
		elseif direction == "down" then
			f.x = (max.x + (max.w / 8)) + 6
			f.w = (max.w * 3 / 4) - 12
		end
		f.y = max.y + 6
		f.h = max.h - 12
		win:setFrame(f, 0.0)
	end
end
hs.hotkey.bind(hyper, "Left", move_window("left"))
hs.hotkey.bind(hyper, "Right", move_window("right"))
hs.hotkey.bind(hyper, "Up", move_window("up"))
hs.hotkey.bind(hyper, "Down", move_window("down"))
