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
		-- spaces_api.changeToSpace(spaces[space], false)
		hs.eventtap.event.newKeyEvent(hs.keycodes.map.cmd, true):post()
		hs.eventtap.event.newKeyEvent(hs.keycodes.map.alt, true):post()
		hs.eventtap.event.newKeyEvent(hs.keycodes.map.ctrl, true):post()
		hs.eventtap.event.newKeyEvent(hs.keycodes.map.shift, true):post()
		hs.eventtap.event.newKeyEvent(
			hs.keycodes.map["" .. space .. ""],
			true
		):post()
		hs.eventtap.event.newKeyEvent(
			hs.keycodes.map["" .. space .. ""],
			false
		):post()
		hs.eventtap.event.newKeyEvent(hs.keycodes.map.cmd, false):post()
		hs.eventtap.event.newKeyEvent(hs.keycodes.map.alt, false):post()
		hs.eventtap.event.newKeyEvent(hs.keycodes.map.ctrl, false):post()
		hs.eventtap.event.newKeyEvent(hs.keycodes.map.shift, false):post()
	end
end
local function get_spaces()
	local layout = spaces_api.layout()
	for k in pairs(layout) do
		monitorId = k
	end
	return layout[monitorId]
end

local function get_current_space()
	local current_space = spaces_api.activeSpace()
	return hs.fnutils.indexOf(spaces, current_space)
end

local function change_to_nearby_space(direction)
	return function()
		local current_space = get_current_space()
		if current_space == nil then return end
		if (current_space == 1 and direction < 0) or (current_space == 9 and direction > 0) then return end
		spaces_api.changeToSpace(spaces[current_space + direction], false)
	end
end

spaces = get_spaces()

return {
	create_missing_space = create_missing_space,
	change_to_space = change_to_space,
	change_to_nearby_space = change_to_nearby_space,
	send_to_space = send_to_space,
}
