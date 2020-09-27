--luacheck: globals hs
local spaces = {}
local spaces_api = require("hs._asm.undocumented.spaces")
local monitorId = ""
local function create_missing_space(space)
	local count = space - #spaces
	for _i = 1, count do
		spaces[#spaces + 1] = spaces_api.createSpace(monitorId)
	end
	return spaces[space]
end
local function send_to_space(space)
	return function()
		local win = hs.window.focusedWindow()
		if (#spaces <= space) then
			win:spacesMoveTo(create_missing_space(space))
		else
			win:spacesMoveTo(spaces[space])
		end
	end
end
local function change_to_space(space)
	return function()
		if (#spaces <= space) then
			create_missing_space(space)
		end
		spaces_api.changeToSpace(spaces[space], false)
	end
end
local function get_spaces()
	local layout = spaces_api.layout()
	for k in pairs(layout) do
		monitorId = k
	end
	return layout[monitorId]
end

spaces = get_spaces()

return {
	change_to_space = change_to_space,
	send_to_space = send_to_space,
}
