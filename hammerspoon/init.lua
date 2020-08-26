--luacheck: globals hs
-- Load libraries:
ext = {
	utils = {},
	watchers = {},
}
ext.utils.window_movements = require("window_movements")
ext.utils.string_literal = require("string_literal")
--- Shortcut keys:
hyper = { "ctrl", "alt", "cmd", "shift" }
mash = { "ctrl", "alt", "cmd" }
mashshift = { "ctrl", "alt", "shift" }

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
local applicationHyperkeys = {
	k = "Kitty",
	f = "Firefox",
}
for key, app in pairs(applicationHyperkeys) do
	hs.hotkey.bind(hyper, key, function()
		hs.application.launchOrFocus(app)
	end)
end
-- autoreload hammerspoon
function ext.utils.reloadConfig()
	-- stop watchers to avoid leaks
	hs.fnutils.each(ext.watchers, function(watcher)
		watcher:stop()
	end)

	hs.reload()
end
-- Auto-reload configuration:
hs.hotkey.bind(hyper, "0", ext.utils.reloadConfig)
ext.watchers.patchwatcher =
	hs.pathwatcher.new(
		os.getenv("HOME") .. "/.hammerspoon/",
		ext.utils.reloadConfig
	):start()
hs.alert.show("Config loaded")
