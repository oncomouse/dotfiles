local beautiful = require("beautiful")
local awful = require("awful")
local is_laptop = require("utils.is_laptop")
beautiful.rules = {
	-- All clients will match this rule.
	{
		id = "global",
		rule = {},
		properties = {
			focus = awful.client.focus.filter,
			raise = true,
			screen = awful.screen.preferred,
			placement = awful.placement.no_overlap + awful.placement.no_offscreen,
		},
	},
	-- Floating clients.
	{
		id = "floating",
		rule_any = {
			instance = { "copyq", "pinentry" },
			class = {
				"mpv",
				"zoom",
				"Thunar",
				"Pcmanfm",
			},
			-- Note that the name property shown in xprop might be set slightly after creation of the client
			-- and the name shown there might not match defined rules here.
			name = {
				"Event Tester", -- xev.
			},
			role = {
				"AlarmWindow", -- Thunderbird's calendar.
				"ConfigManager", -- Thunderbird's about:config.
				"pop-up", -- e.g. Google Chrome's (detached) Developer Tools.
			},
		},
		properties = { floating = true },
	},
	{
		id = "floating-utilities",
		rule_any = {
			type = {
				"dialog",
				"utility",
				"toolbar",
				"splash",
			},
		},
		properties = {
			floating = true,
			modal = true,
		},
	},
	{
		id = "titlbar-test",
		rule_any = {
			type = {
				"normal",
				"dialog",
			},
		},
		properties = {
			can_titlebar = true,
		},
	},
}

if is_laptop then
	beautiful.rules[1].properties.floating = true
end
