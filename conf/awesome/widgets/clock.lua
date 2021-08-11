local block = require("utils.block")
local clock_widget = {}

local function create()
	return block("date +'%a %m/%d %-I:%M %p '",0.1)
end
return setmetatable(clock_widget, { __call = function(_, ...)
	return create(...)
end })
