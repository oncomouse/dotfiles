--luacheck: globals hs
-- Load libraries:
window_movements = require("window_movements")
string_literal = require("string_literal")
--- Shortcut keys:
hyper = { "ctrl", "alt", "cmd", "shift" }
mash = { "ctrl", "alt", "cmd" }
-- mashshift = { "ctrl", "alt", "shift" }

-- Summon console:
hs.hotkey.bindSpec({ mash, "space" }, hs.toggleConsole)

-- Window management:
hs.window.animationDuration = 0
-- Halves:
hs.hotkey.bindSpec({ hyper, "left" }, window_movements.left)
hs.hotkey.bindSpec({ hyper, "right" }, window_movements.right)
-- Maximize:
hs.hotkey.bindSpec({ hyper, "up" }, window_movements.up)
-- Custom "reading" window:
hs.hotkey.bindSpec({ hyper, "down" }, window_movements.down)
-- Quarters:
hs.hotkey.bindSpec({ mash, "left" }, window_movements.up_left)
hs.hotkey.bindSpec({ mash, "right" }, window_movements.down_right)
hs.hotkey.bindSpec({ mash, "up" }, window_movements.up_right)
hs.hotkey.bindSpec({ mash, "down" }, window_movements.down_left)
-- Spotify controls:
function show_spotify_song()
	hs.alert.show(
		string_literal([[$artist - $song

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

-- Auto-reload configuration:
function reloadConfig(files)
	doReload = false
	for _, file in pairs(files) do
		if file:sub(-4) == ".lua" then
			doReload = true
		end
	end
	if doReload then
		hs.reload()
	end
end
hs.hotkey.bind(hyper, "0", function()
	hs.reload()
end)
configWatcher =
	hs.pathwatcher.new(
		os.getenv("HOME") .. "/.hammerspoon/",
		reloadConfig
	):start()
hs.alert.show("Config loaded")
