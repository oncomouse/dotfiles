--luacheck: globals hs
-- Load libraries:
ext = {
	utils = {},
	watchers = {},
	modals = {},
}
ext.utils.window_movements = require("utils/window_movements")
ext.utils.string_literal = require("utils/string_literal")
ext.utils.reload_config = require("utils/reload_config")
--- Shortcut keys:
hyper = { "ctrl", "alt", "cmd", "shift" }
mash = { "ctrl", "alt", "cmd" }
mashshift = { "ctrl", "alt", "shift" }
-- shortmash = {"ctrl", "cmd"}

-- Summon console:
hs.hotkey.bindSpec({ mash, "space" }, hs.toggleConsole)

-- Window management:
hs.window.animationDuration = 0
-- Halves:
hs.hotkey.bindSpec({ hyper, "left" }, ext.utils.window_movements.left)
hs.hotkey.bindSpec({ hyper, "right" }, ext.utils.window_movements.right)
-- Maximize:
hs.hotkey.bindSpec({ hyper, "up" }, ext.utils.window_movements.up)
-- Custom "reading" window:
hs.hotkey.bindSpec({ hyper, "down" }, ext.utils.window_movements.down)
-- Quarters:
hs.hotkey.bindSpec({ mash, "left" }, ext.utils.window_movements.up_left)
hs.hotkey.bindSpec({ mash, "right" }, ext.utils.window_movements.down_right)
hs.hotkey.bindSpec({ mash, "up" }, ext.utils.window_movements.up_right)
hs.hotkey.bindSpec({ mash, "down" }, ext.utils.window_movements.down_left)
-- Livestreaming:
hs.hotkey.bindSpec({ mashshift, "left" }, ext.utils.window_movements.gif_window)
-- Spotify controls:
function show_spotify_song()
	hs.alert.show(
		ext.utils.string_literal([[$artist - $song

*$album*]], {
			artist = hs.spotify.getCurrentArtist(),
			album = hs.spotify.getCurrentAlbum(),
			song = hs.spotify.getCurrentTrack(),
		}),
		{
			textStyle = {
				paragraphStyle = { alignment = "center" },
			},
		}
	)
end
hs.hotkey.bindSpec({ hyper, "space" }, show_spotify_song)

-- Date & time:
function show_date()
	hs.alert.show(os.date([[%B %m, %Y

%H:%M]]), {
		textStyle = {
			paragraphStyle = { alignment = "center" },
		},
	})
end
hs.hotkey.bindSpec({ hyper, "d" }, show_date)
-- Open Email:
hs.hotkey.bindSpec({ hyper, "m" }, function()
	hs.urlevent.openURL("https://mail.google.com")
end)
-- App Switcher:
-- hyper + a loads app switch mode, then key triggers app:
local application_hyperkeys = {
	k = "Kitty",
	f = { { "", "Firefox" }, { "shift", "Finder" } },
	v = "VimR",
}
ext.modals.app_switcher = hs.hotkey.modal.new(hyper, "a")
for key, app in pairs(application_hyperkeys) do
	if type(app) == "table" then
		for _k, mod_app in pairs(app) do
			ext.modals.app_switcher:bind(mod_app[1], key, function()
				hs.application.launchOrFocus(mod_app[2])
				ext.modals.app_switcher:exit()
			end)
		end
	else
		ext.modals.app_switcher:bind("", key, function()
			hs.application.launchOrFocus(app)
			ext.modals.app_switcher:exit()
		end)
	end
end
ext.modals.app_switcher:bind("", "escape", function()
	ext.modals.app_switcher:exit()
end)
-- hyper + a, ? shows list of apps
ext.modals.app_switcher:bind("shift", "/", function()
	hs.alert.show(hs.inspect.inspect(application_hyperkeys))
	ext.modals.app_switcher:exit()
end)
function ext.modals.app_switcher:entered()
	hs.timer.doAfter(10, function()
		self:exit()
	end)
end
-- Reload configuration:
hs.hotkey.bind(hyper, "0", function()
	hs.reload()
end)
-- Auto-reload configuration:
ext.watchers.patchwatcher =
	hs.pathwatcher.new(
		os.getenv("HOME") .. "/.hammerspoon/",
		ext.utils.reload_config
	):start()
hs.alert.show("Config loaded")
