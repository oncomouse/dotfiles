-- luacheck: globals awesome mouse
local wibox = require("wibox")
local awful = require("awful")
local gears = require("gears")

-- Setup
local mpris_widget = {}
local dpi = require("beautiful").xresources.apply_dpi

-- Metadata
local icon
local artist
local album
local title
local artUrl

-- Truncate:
function truncate(st, len)
	return string.len(st) > len and string.sub(st, 0, len - 1) .. "…" or st
end

function make_output()
	return artist ~= nil and truncate(string.format("%s %s - %s", icon, artist, title), 30) or ""
end

-- We set this signal connection first, so that if a player is playing when
-- we start Awesome, it will already have metadata ready:
awesome.connect_signal("evil::mpris_widget::metadata", function(ic, ar, tt, al, au)
	icon = ic
	artist = ar
	album = al
	title = tt
	artUrl = au
	if mpris_widget.widget ~= nil then
		mpris_widget.widget:set_value(make_output())
	end
end)

local function create()
	mpris_widget.widget = wibox.widget{
		{
			{
				id = "title",
				text = make_output(),
				widget = wibox.widget.textbox,
			},
			layout = wibox.layout.fixed.horizontal,
		},
		margin2 = 1,
		widget = wibox.container.margin,
		set_value = function(self, output)
			self:get_children_by_id("title")[1]:set_markup(output)
		end
	}
	mpris_widget.widget:connect_signal("button::press", function(self, _, _, button)
		if button == 1 then
			self:play()
		elseif button == 2 then
			self:previous()
		elseif button == 3 then
			self:next()
		end
	end)
	local mpris_tooltip = awful.tooltip {
		mode = 'outside',
		preferred_positions = {'bottom'},
	 }
	mpris_tooltip:add_to_object(mpris_widget.widget)

	local mpris_popup = awful.popup{
		ontop = true,
		visible = false,
		shape = gears.shape.rect,
		-- border_width = 1,
		-- border_color = beautiful.bg_focus,
		maximum_width = dpi(400),
		offset = {y = 5},
		widget = {},
		set_text = function(self, text)
			self:get_children_by_id("text")[1]:set_markup(text)
		end
	}

	local function get_image()
		if artUrl == nil then
			return ''
		end
		if artUrl:sub(1,8) == 'https://' then
			return os.getenv('HOME') .. '/.cache/awesome/mpris.png'
		end
		return artUrl
	end
	mpris_widget.widget:connect_signal('mouse::enter', function()
		if artist ~= nil then
			-- mpris_tooltip.markup = '<b>Artist</b>: ' .. artist
			-- 	.. '\n<b>Song</b>: ' .. title
			-- 	.. '\n<b>Album</b>: ' .. album
			local f = wibox.widget{
				homogeneous = true,
				spacing = 5,
				min_cols_size = 10,
				min_rows_size = 10,
				layout  = wibox.layout.grid,
			}
			f:add_widget_at(wibox.widget{
				widget=wibox.widget.imagebox,
				id="image",
				image= get_image(),
				forced_width=dpi(75),
				forced_height=dpi(75),
			}, 1, 1)
			f:add_widget_at(wibox.widget{
				widget=wibox.widget.textbox,
				id="text",
				markup='<b>Artist</b>: ' .. artist
				.. '\n<b>Song</b>: ' .. title
				.. '\n<b>Album</b>: ' .. album,
			}, 1, 2, 1, 3)
			mpris_popup:setup{
				widget=f
			}
		end
		mpris_popup.visible = true
		mpris_popup:move_next_to(mouse.current_widget_geometry)
	end)
	mpris_widget.widget:connect_signal('mouse::leave', function()
		mpris_popup.visible = false
	end)

	function mpris_widget:play()
		awesome.emit_signal("evil::mpris_widget::change", "play_pause")
	end
	function mpris_widget:previous_track()
		awesome.emit_signal("evil::mpris_widget::change", "previous")
	end
	function mpris_widget:next_track()
		awesome.emit_signal("evil::mpris_widget::change", "next")
	end
	function mpris_widget:stop()
		awesome.emit_signal("evil::mpris_widget::change", "stop")
	end

	return mpris_widget.widget
end

return setmetatable(mpris_widget, { __call = function(_, ...)
	return create(...)
end })
