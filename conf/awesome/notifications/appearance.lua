-- Adapated from: https://github.com/elenapan/dotfiles/blob/master/config/awesome/notifications/themes/amarena.lua
local beautiful = require("beautiful")
local wibox = require("wibox")
local gears = require("gears")
local dpi = beautiful.xresources.apply_dpi

local helpers = require("helpers")
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
local naughty = require("naughty")
naughty.connect_signal("request::display_error", function(message, startup)
	naughty.notification({
		urgency = "critical",
		title = "Oops, an error happened" .. (startup and " during startup!" or "!"),
		message = message,
	})
end)
-- naughty.connect_signal("request::display", function(n)
-- 	naughty.layout.box({ notification = n })
-- end)
-- Note: This theme does not show image notification icons

-- For antialiasing
-- The real background color is set in the widget_template
beautiful.notification_bg = "#00000000"

local default_icon = "𥉉"

-- Custom text icons according to the notification's app_name
-- plus whether the title should be visible or not
-- (This will be removed when notification rules are released)
-- Using Nerd Font
local app_config = {
	["date"] = { icon = "", title = true },
	["battery"] = { icon = "", title = false },
	["charger"] = { icon = "ﮣ", title = false },
	["volume"] = { icon = "墳", title = false },
	["brightness"] = { icon = "", title = false },
	["screenshot"] = { icon = "", title = false },
	["Telegram Desktop"] = { icon = "切", title = true },
	["night_mode"] = { icon = "望", title = false },
	["NetworkManager"] = { icon = "ﯱ", title = true },
	["youtube"] = { icon = "輸", title = true },
	["mpris"] = { icon = "ﱘ", title = true },
	["mpv"] = { icon = "", title = true },
	["keyboard"] = { icon = "", title = false },
	["email"] = { icon = "", title = true },
}

local urgency_color = {
	["low"] = beautiful.color2,
	["normal"] = beautiful.color4,
	["critical"] = beautiful.color11,
}

-- Template
-- ===================================================================
naughty.connect_signal("request::display", function(n)
	-- Custom icon widget
	-- It can be used instead of naughty.widget.icon if you prefer your icon to be
	-- a textbox instead of an image. However, you have to determine its
	-- text/markup value from the notification before creating the
	-- naughty.layout.box.
	local custom_notification_icon = wibox.widget({
		font = "FiraCode Nerd Font 18",
		align = "center",
		valign = "center",
		widget = wibox.widget.textbox,
	})

	local icon, title_visible
	local color = urgency_color[n.urgency]
	-- Set icon according to app_name
	if app_config[n.app_name] then
		icon = app_config[n.app_name].icon
		title_visible = app_config[n.app_name].title
	else
		icon = default_icon
		title_visible = true
	end

	local actions = wibox.widget({
		notification = n,
		base_layout = wibox.widget({
			spacing = dpi(3),
			layout = wibox.layout.flex.horizontal,
		}),
		widget_template = {
			{
				{
					{
						id = "text_role",
						font = beautiful.notification_font,
						widget = wibox.widget.textbox,
					},
					left = dpi(6),
					right = dpi(6),
					widget = wibox.container.margin,
				},
				widget = wibox.container.place,
			},
			bg = beautiful.color8 .. "32",
			forced_height = dpi(25),
			forced_width = dpi(70),
			widget = wibox.container.background,
		},
		style = {
			underline_normal = false,
			underline_selected = true,
		},
		widget = naughty.list.actions,
	})

	naughty.layout.box({
		notification = n,
		type = "notification",
		-- For antialiasing: The real shape is set in widget_template
		shape = gears.shape.rectangle,
		border_width = beautiful.notification_border_width,
		border_color = urgency_color[n.urgency],
		position = beautiful.notification_position,
		widget_template = {
			{
				{
					{
						{
							markup = helpers.colorize_text(icon, color),
							align = "center",
							valign = "center",
							widget = custom_notification_icon,
						},
						forced_width = dpi(50),
						bg = beautiful.background,
						widget = wibox.container.background,
					},
					{
						{
							{
								align = "center",
								visible = title_visible,
								font = beautiful.notification_font,
								markup = "<b>" .. n.title .. "</b>",
								widget = wibox.widget.textbox,
								-- widget = naughty.widget.title,
							},
							{
								align = "center",
								-- wrap = "char",
								widget = naughty.widget.message,
							},
							{
								helpers.vertical_pad(dpi(10)),
								{
									actions,
									shape = helpers.rrect(dpi(4)),
									widget = wibox.container.background,
								},
								visible = n.actions and #n.actions > 0,
								layout = wibox.layout.fixed.vertical,
							},
							layout = wibox.layout.align.vertical,
						},
						margins = beautiful.notification_margin,
						widget = wibox.container.margin,
					},
					layout = wibox.layout.fixed.horizontal,
				},
				strategy = "max",
				width = beautiful.notification_max_width or dpi(350),
				height = beautiful.notification_max_height or dpi(180),
				widget = wibox.container.constraint,
			},
			-- Anti-aliasing container
			shape = helpers.rrect(beautiful.notification_border_radius),
			bg = beautiful.color0,
			widget = wibox.container.background,
		},
	})
end)
