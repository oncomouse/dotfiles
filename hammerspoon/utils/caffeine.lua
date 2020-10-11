-- luacheck: globals hs
local caffeineMenuBarItem = hs.menubar.new()
--- How long (in seconds) to let caffeine run:
local caffeineTimer = nil
function setCaffeineDisplay(state, timeOut)
	if state then
		caffeineMenuBarItem:setIcon("caffeine-on.pdf")
		caffeineTimer = hs.timer.doAfter(timeOut, turnOffCaffeine(timeOut))
	else
		if caffeineTimer ~= nil then
			caffeineTimer:stop()
		end
		caffeineMenuBarItem:setIcon("caffeine-off.pdf")
	end
end

function turnOffCaffeine(timeOut)
	return function()
		if hs.caffeinate.get("displayIdle") then
			setCaffeineDisplay(hs.caffeinate.toggle("displayIdle"), timeOut)
		end
	end
end

function caffeineClicked(timeOut)
	return function()
		setCaffeineDisplay(hs.caffeinate.toggle("displayIdle"), timeOut)
	end
end

function caffeineConfig(timeOut)
	local caffeineCallback = caffeineClicked(timeOut)
	if caffeineMenuBarItem then
		caffeineMenuBarItem:setClickCallback(caffeineCallback)
		setCaffeineDisplay(hs.caffeinate.get("displayIdle"))
	end
	return caffeineCallback
end

return caffeineConfig
