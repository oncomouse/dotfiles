-- luacheck: globals client
-- Animate active borders, modified from: https://www.reddit.com/r/awesomewm/comments/mmt4ms/colour_cycling_active_window_border_snippet/
local gears = require("gears")
-- Gradient generator, adapted from https://krazydad.com/tutorials/makecolors.php
function make_color_gradient(frequency1, frequency2, frequency3, phase1, phase2, phase3, center, width, len)
	local output = {}
	center = center and center or 128
	width = width and width or 127
	len = len and len or 120
	for gen_loop = 1, len do
		red = string.format("%02x", math.floor(math.sin(frequency1 * gen_loop + phase1) * width + center))
		grn = string.format("%02x", math.floor(math.sin(frequency2 * gen_loop + phase2) * width + center))
		blu = string.format("%02x", math.floor(math.sin(frequency3 * gen_loop + phase3) * width + center))
		output[gen_loop] = "#" .. red .. grn .. blu
	end
	return output
end

local red_frequency = .11
local grn_frequency = .13
local blu_frequency = .17

local phase1 = 0
local phase2 = 10
local phase3 = 30

local center = 180
local width = 40
local len = 80

local border_animate_colours = make_color_gradient(red_frequency, grn_frequency, blu_frequency, phase1, phase2, phase3, center, width, len)

local border_loop = 1
local border_loop_reverse = false
local border_timer = gears.timer{
	timeout = 0.03,
	call_now = false,
	autostart = false,
	callback = function()
		local c = client.focus
		if c then
			c.border_color = border_animate_colours[border_loop]
			if not border_loop_reverse then
				border_loop = border_loop + 1
				if border_loop >= len then
					border_loop_reverse = true
				end
			end
			if border_loop_reverse then
				border_loop = border_loop - 1
				if border_loop <= 1 then
					border_loop_reverse = false
				end
			end
		end
	end,
}

-- Start timer if a floating window appears and has focus:
client.connect_signal("manage", function(c)
	if c == client.focus then
		if not border_timer.started then
			border_timer:start()
		end
	end
end)

client.connect_signal("focus", function(c)
	if not border_timer.started then
		border_timer:start()
	end
	c.border_color = border_animate_colours[border_loop]
end)

client.connect_signal("unfocus", function(_)
	if border_timer.started then
		border_timer:stop()
	end
end)

-- The solo no-border util emits a signal that will turn off gradient changing
client.connect_signal("border_gradient::solo", function(_)
	if border_timer.started then
		border_timer:stop()
	end
end)
