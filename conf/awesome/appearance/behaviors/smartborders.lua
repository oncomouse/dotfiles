-- luacheck: globals screen tag client
local beautiful = require("beautiful")
local truefloat = require("appearance.utils.truefloat")
local function make_border_dwim(t)
	-- use this because there might be multiple tag selected
	local cs = t.screen.clients
	local border = {}

	-- TODO: look up table instead of ifelse
	if t.layout.name == "max" then
		for _, c in ipairs(cs) do
			if truefloat(c) then
				border[c] = true
			else
				border[c] = false
			end
		end
	elseif t.layout.name == "floating" then
		for _, c in ipairs(cs) do
			if c.maximized or c.fullscreen then
				border[c] = false
			else
				border[c] = true
			end
		end
	else
		local count = 0
		local only = nil
		for _, c in ipairs(cs) do
			if truefloat(c) then
				border[c] = true
			elseif c.maximized or c.fullscreen or c.minimized then
				border[c] = false
			else
				count = count + 1
				only = c
				border[c] = true
			end
		end
		if count == 1 then
			border[only] = false
		end
	end

	for c, bd in pairs(border) do
		if bd then
			c.border_width = beautiful.border_width
		else
			c.border_width = 0
		end
	end
end

local function make_border_dwim_screen_wrapper(s)
	local t = s.selected_tag
	-- no tag selected
	if t == nil then
		return
	end
	make_border_dwim(t)
end

local function make_border_dwim_client_wrapper(c)
	-- don't use c.first_tag or c.tags because unmanaged client have no tag
	-- so if either of those used, in a tile layout
	-- when a client is killed and there is one left, the border will still be there
	local s = c.screen
	if s then
		make_border_dwim_screen_wrapper(s)
	end
end

client.connect_signal("tagged", make_border_dwim_client_wrapper)
client.connect_signal("untagged", make_border_dwim_client_wrapper)
client.connect_signal("unmanage", make_border_dwim_client_wrapper)
client.connect_signal("property::floating", make_border_dwim_client_wrapper)
client.connect_signal("property::maximized", make_border_dwim_client_wrapper)
client.connect_signal("property::fullscreen", make_border_dwim_client_wrapper)
client.connect_signal("property::minimized", make_border_dwim_client_wrapper)
tag.connect_signal("property::layout", make_border_dwim)
screen.connect_signal("tag::history::update", make_border_dwim_screen_wrapper)

