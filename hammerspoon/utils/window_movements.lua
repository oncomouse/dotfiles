-- luacheck: globals hs
local window_movements = {}

window_movements.left = function()
	local win = hs.window.focusedWindow()
	if not win then return end
	win:moveToUnit(hs.layout.left50)
end

window_movements.right = function()
	local win = hs.window.focusedWindow()
	if not win then return end
	win:moveToUnit(hs.layout.right50)
end

window_movements.up = function()
	local win = hs.window.focusedWindow()
	if not win then return end
	win:moveToUnit(hs.layout.maximized)
end

-- Create a nice centered reading window:
window_movements.down = function()
	local win_percentage = .95
	local offset = (1 - win_percentage) / 2
	local win = hs.window.focusedWindow()
	if not win then return end
	win:moveToUnit(
		hs.geometry.rect(offset, offset, win_percentage, win_percentage)
	)
end

window_movements.up_left = function()
	local win = hs.window.focusedWindow()
	if not win then return end
	win:moveToUnit(hs.geometry.rect(0, 0, 0.5, 0.5))
end

window_movements.up_right = function()
	local win = hs.window.focusedWindow()
	if not win then return end
	win:moveToUnit(hs.geometry.rect(0.5, 0, 0.5, 0.5))
end

window_movements.down_left = function()
	local win = hs.window.focusedWindow()
	if not win then return end
	win:moveToUnit(hs.geometry.rect(0, 0.5, 0.5, 0.5))
end

window_movements.down_right = function()
	local win = hs.window.focusedWindow()
	if not win then return end
	win:moveToUnit(hs.geometry.rect(0.5, 0.5, 0.5, 0.5))
end

window_movements.gif_window = function()
	local win = hs.window.focusedWindow()
	if not win then return end
	win:setSize({
		w = 900,
		h = 618,
	})
end

return window_movements
