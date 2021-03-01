-- luacheck: globals client tag
local awful = require("awful")
local beautiful = require("beautiful")
-- Source: https://github.com/actionless/awesome_config/blob/master/actionless/util/tag.lua
local function get_tiled(t)
	local tiled_clients = {}

	-- @TODO: add some fix for sticky clients: DONE?
	if not t then
		return tiled_clients
	end
	local s = t.screen
	if s.selected_tags and #s.selected_tags > 1 then
		return s.tiled_clients
	end

	local visible_clients = s.tiled_clients
	local clients_on_tag = t:clients()
	for _, c in pairs(visible_clients) do
		if c.valid and c.sticky then
			table.insert(tiled_clients, c)
		end
	end
	for _, c in pairs(clients_on_tag) do
		if
			not c.floating and
			not c.fullscreen and
			not c.maximized_vertical and
			not c.maximized_horizontal and
			not c.minimized and
			not c.sticky then
			table.insert(tiled_clients, c)
		end
	end
	return tiled_clients
end
client.connect_signal("focus", function(c)
	c.border_color = beautiful.border_focus
end)
client.connect_signal("unfocus", function(c)
	c.border_color = beautiful.border_normal
end)
-- No borders if only client in tile mode:
function update_borders(c)
	update_tag_borders(c.first_tag or awful.screen.focused().selected_tag)
end
function update_tag_borders(_)
	local tags = awful.screen.focused().selected_tags
	local clients = {}
	for _, t in pairs(tags) do
		for _, c in pairs(get_tiled(t)) do
			table.insert(clients, c)
		end
	end
	local b = beautiful.border_width
	if #clients == 1 then
		b = 0
	end
	for _, c in pairs(clients) do
		c.border_width = b
	end
end
client.connect_signal("manage", update_borders)
client.connect_signal("unmanage", update_borders)
client.connect_signal("tagged", update_borders)
client.connect_signal("untagged", update_borders)
tag.connect_signal("property::layout", update_tag_borders)
tag.connect_signal("property::selected", update_tag_borders)

