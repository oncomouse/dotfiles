-- Source: https://github.com/elenapan/dotfiles/blob/master/config/awesome/decorations/themes/ephemeral.lua
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local gears = require("gears")
local helpers = require("helpers")
local decorations = require("appearance.titlebars.decorations")
local truefloat = require("appearance.utils.truefloat")
local should_float = require("appearance.utils.should_float")

local xrdb = beautiful.xresources.get_current_theme()
-- Make dpi function global
local dpi = beautiful.xresources.apply_dpi
-- Button configuration
local gen_button_size = dpi(9)
local gen_button_margin = dpi(8)
local gen_button_color_unfocused = xrdb.color8
local gen_button_shape = gears.shape.circle

-- Add a titlebar
client.connect_signal("request::titlebars", function(c)
	local buttons = {
		awful.button({}, awful.button.names.LEFT, function()
			c:activate({ context = "titlebar", action = "mouse_move" })
		end),
		awful.button({}, awful.button.names.RIGHT, function()
			c:activate({ context = "titlebar", action = "mouse_resize" })
		end),
	}
	awful.titlebar(
		c,
		{ font = beautiful.titlebar_font, position = beautiful.titlebar_position, size = beautiful.titlebar_size }
	):setup({
		nil,
		{
			buttons = buttons,
			font = beautiful.titlebar_font,
			align = beautiful.titlebar_title_align or "center",
			widget = beautiful.titlebar_title_enabled and awful.titlebar.widget.titlewidget(c) or wibox.widget.textbox(
				""
			),
		},
		{
			-- Generated buttons
			decorations.button(
				c,
				gen_button_shape,
				xrdb.color3,
				gen_button_color_unfocused,
				xrdb.color11,
				gen_button_size,
				gen_button_margin,
				"sticky"
			),
			decorations.button(
				c,
				gen_button_shape,
				xrdb.color2,
				gen_button_color_unfocused,
				xrdb.color10,
				gen_button_size,
				gen_button_margin,
				"maximize"
			),
			decorations.button(
				c,
				gen_button_shape,
				xrdb.color5,
				gen_button_color_unfocused,
				xrdb.color13,
				gen_button_size,
				gen_button_margin,
				"close"
			),
			-- Create some extra padding at the edge
			helpers.horizontal_pad(gen_button_margin / 2),

			layout = wibox.layout.fixed.horizontal,
		},
		layout = wibox.layout.align.horizontal,
	})
end)

local function set_titlebar(client, s)
	if not client.can_titlebar then
		return
	end
	if s then
		if client.titlebar == nil then
			client:emit_signal("request::titlebars", "rules", {})
		end
		awful.titlebar.show(client)
	else
		awful.titlebar.hide(client)
	end
end

local function handle_adding_titlebars(c)
	set_titlebar(c, truefloat(c) or (c.first_tag and c.first_tag.layout.name == "floating"))
end

-- Hook called when a client spawns
client.connect_signal("request::manage", handle_adding_titlebars)
client.connect_signal("property::floating", handle_adding_titlebars)

tag.connect_signal("property::layout", function(t)
	for _, c in pairs(t:clients()) do
		if t.layout == awful.layout.suit.floating then
			set_titlebar(c, true)
		else
			if not should_float(c) then
				set_titlebar(c, false)
			end
		end
	end
end)

