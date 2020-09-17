-- luacheck: globals hs
local caffeine = hs.menubar.new()
function setCaffeineDisplay(state)
	if state then
		caffeine:setIcon("caffeine-on.pdf")
	else
		caffeine:setIcon("caffeine-off.pdf")
	end
end

function caffeineClicked()
	setCaffeineDisplay(hs.caffeinate.toggle("displayIdle"))
end

if caffeine then
	caffeine:setClickCallback(caffeineClicked)
	setCaffeineDisplay(hs.caffeinate.get("displayIdle"))
end
return caffeineClicked
