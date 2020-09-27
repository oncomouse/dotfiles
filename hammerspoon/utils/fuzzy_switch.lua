-- luacheck: globals hs
-- Fuzzy Window Switcher
local spaces = require("hs._asm.undocumented.spaces.")

local _fuzzyChoices = nil
local _fuzzyChooser = nil
local _fuzzyLastWindow = nil

local function fuzzyQuery(s, m)
	local s_index = 1
	local m_index = 1
	local match_start = nil
	while true do
		if s_index > s:len() or m_index > m:len() then
			return -1
		end
		local s_char = s:sub(s_index, s_index)
		local m_char = m:sub(m_index, m_index)
		if s_char == m_char then
			if match_start == nil then
				match_start = s_index
			end
			s_index = s_index + 1
			m_index = m_index + 1
			if m_index > m:len() then
				match_end = s_index
				s_match_length = match_end - match_start
				score = m:len() / s_match_length
				return score
			end
		else
			s_index = s_index + 1
		end
	end
end

local function _fuzzyFilterChoices(query)
	if query:len() == 0 then
		_fuzzyChooser:choices(_fuzzyChoices)
		return
	end
	local pickedChoices = {}
	for _i, choice in pairs(_fuzzyChoices) do
		local fullText = (choice["text"] .. " " .. choice["subText"]):lower()
		local score = fuzzyQuery(fullText, query:lower())
		if score > 0 then
			choice["fzf_score"] = score
			table.insert(pickedChoices, choice)
		end
	end
	local sort_func = function(a, b)
		return a["fzf_score"] > b["fzf_score"]
	end
	table.sort(pickedChoices, sort_func)
	_fuzzyChooser:choices(pickedChoices)
end

local function _fuzzyPickWindow(item)
	if item == nil then
		if _fuzzyLastWindow then
			-- Workaround so last focused window stays focused after dismissing
			_fuzzyLastWindow:focus()
			_fuzzyLastWindow = nil
		end
		return
	end
	local id = item["windowID"]
	local currentSpace = spaces.activeSpace()
	local windowSpaces = spaces.windowOnSpaces(id)
	if not hs.fnutils.contains(windowSpaces, currentSpace) then
		spaces.changeToSpace(windowSpaces[1])
	end
	local window = hs.window.get(id)
	window:focus()
end

local apps_to_skip = { "UnmountAssistantAgent" }

local function windowFuzzySearch()
	local windows =
		hs.window.filter.default:getWindows(hs.window.filter.sortByFocusedLast)
	-- windows = hs.window.orderedWindows()
	_fuzzyChoices = {}
	for i, w in pairs(windows) do
		local title = w:title()
		local app = w:application():name()
		local item = {
			text = app,
			subText = title,
			windowID = w:id(),
		}
		-- Anything we want to skip?
		if not hs.fnutils.contains(apps_to_skip, app) then
			table.insert(_fuzzyChoices, item)
		end
	end
	_fuzzyLastWindow = hs.window.focusedWindow()
	_fuzzyChooser =
		hs.chooser.new(_fuzzyPickWindow):choices(_fuzzyChoices):searchSubText(
			true
		)
	_fuzzyChooser:queryChangedCallback(_fuzzyFilterChoices) -- Enable true fuzzy find
	_fuzzyChooser:show()
end

return windowFuzzySearch
