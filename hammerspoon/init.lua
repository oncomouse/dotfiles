--luacheck: globals hs
-- Load libraries:
_ = {
	utils = {},
	watchers = {},
	modals = {},
	--- Shortcut keys:
	mods = {
		hyper = { "ctrl", "alt", "cmd", "shift" },
		mash = { "ctrl", "alt", "cmd" },
		mashshift = { "ctrl", "alt", "shift" },
		-- shortmash = {"ctrl", "cmd"},
	},
	hotkeys = {
		-- hyper + a -> _ to launch:
		application_quickkeys = {
			k = "Kitty",
			f = { { "", "Firefox" }, { "shift", "Finder" } },
			v = "VimR",
			z = "Zotero",
		},
	},
}
_.utils.window_movements = require("utils/window_movements")
_.utils.string_literal = require("utils/string_literal")
_.utils.reload_config = require("utils/reload_config")
_.utils.show_spotify_song = require("utils/show_spotify_song")
_.utils.show_date = require("utils/show_date")
_.utils.make_app_switcher = require("utils/app_switcher")

-- Summon console:
hs.hotkey.bindSpec({ _.mods.mash, "space" }, hs.toggleConsole)

-- Window management:
hs.window.animationDuration = 0
-- Halves:
hs.hotkey.bindSpec({ _.mods.hyper, "left" }, _.utils.window_movements.left)
hs.hotkey.bindSpec({ _.mods.hyper, "right" }, _.utils.window_movements.right)
-- Maximize:
hs.hotkey.bindSpec({ _.mods.hyper, "up" }, _.utils.window_movements.up)
-- Custom "reading" w_.mods.indow:
hs.hotkey.bindSpec({ _.mods.hyper, "down" }, _.utils.window_movements.down)
-- Quarters:
hs.hotkey.bindSpec({ _.mods.mash, "left" }, _.utils.window_movements.up_left)
hs.hotkey.bindSpec(
	{ _.mods.mash, "right" },
	_.utils.window_movements.down_right
)
hs.hotkey.bindSpec({ _.mods.mash, "up" }, _.utils.window_movements.up_right)
hs.hotkey.bindSpec({ _.mods.mash, "down" }, _.utils.window_movements.down_left)
-- Livestreaming:
hs.hotkey.bindSpec(
	{ _.mods.mashshift, "left" },
	_.utils.window_movements.gif_window
)
-- Spotify controls:
hs.hotkey.bindSpec({ _.mods.hyper, "space" }, _.utils.show_spotify_song)

hs.hotkey.bindSpec({ _.mods.hyper, "d" }, _.utils.show_date)
hs.hotkey.bindSpec({ _.mods.hyper, "m" }, function()
	hs.urlevent.openURL("https://mail.google.com")
end)

_.modals.app_switcher =
	_.utils.make_app_switcher(
		_.mods.hyper,
		"a",
		_.hotkeys.application_quickkeys
	)
-- Reload configuration:
hs.hotkey.bind(_.mods.hyper, "0", function()
	hs.reload()
end)
-- Auto-reload configuration:
_.watchers.patchwatcher =
	hs.pathwatcher.new(
		os.getenv("HOME") .. "/.hammerspoon/",
		_.utils.reload_config
	):start()
hs.alert.show("Config loaded")
