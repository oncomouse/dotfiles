--luacheck: globals hs
-- _ is the universal configuration object:
local _ = {}
-- Load external libraries:
_.utils = {
	window_movements = require("utils/window_movements"),
	string_literal = require("utils/string_literal"),
	reload_config = require("utils/reload_config"),
	show_spotify_song = require("utils/show_spotify_song"),
	show_date = require("utils/show_date"),
	make_modal = require("utils/make_modal"),
	bind_generator = require("utils/bind_generator"),
}
--- Modifier keys:
_.mods = {
	hyper = { "ctrl", "alt", "cmd", "shift" },
	mash = { "ctrl", "alt", "cmd" },
	mashshift = { "ctrl", "alt", "shift" },
	-- shortmash = {"ctrl", "cmd"},
	-- ctrl = { "ctrl" },
	-- cmd = { "cmd" },
	-- alt = { "alt" },
}
-- Shortcuts:
_.hot_keys = {
	-- No modifiers:
	_ = {},
	-- Hammerspoon console:
	mash = { space = hs.toggleConsole },
	hyper = {
		-- Show current Spotify song:
		space = _.utils.show_spotify_song,
		-- Show date:
		d = _.utils.show_date,
	},
}
-- Groups of hot_keys:
_.modal_keys = {
	hyper = {
		-- Modal app launcher:
		a = {
			f = { { "", "Firefox" }, { _.mods.hyper, "Finder" } },
			m = { { "", "Messages" }, { _.mods.hyper, function()
				hs.urlevent.openURL("https://mail.google.com")
			end } }, -- Open Mail with hyper+a, hyper+m
			k = "Kitty",
			s = "Spotify",
			v = "VimR",
			z = "Zotero",
		},
		-- Modal window movements:
		-- hyper + w -> c = center
		-- hyper + w -> 1-4 = quarters
		-- hyper + w -> <-/-> = halves
		-- hyper + w -> up = maximize
		-- hyper + w -> down = reading
		-- hyper + w -> 5 = animated GIF capture window (900 x 618)
		w = {
			[1] = _.utils.window_movements.up_left,
			[2] = _.utils.window_movements.up_right,
			[3] = _.utils.window_movements.down_right,
			[4] = _.utils.window_movements.down_left,
			[5] = _.utils.window_movements.gif_window,
			c = _.utils.window_movements.center_on_screen,
			left = _.utils.window_movements.left,
			right = _.utils.window_movements.right,
			up = _.utils.window_movements.up,
			down = _.utils.window_movements.down,
		},
	},
}
_.watchers = {}
_.modals = {}
_.generators = {
	hotkeys = _.utils.bind_generator(_.mods, hs.hotkey.bind),
	modals = _.utils.bind_generator(_.mods, _.utils.make_modal),
}

-- Window management:
hs.window.animationDuration = 0

-- Hook up shortcuts:
_.generators.hotkeys(_.hot_keys)
_.generators.modals(_.modal_keys)

-- Auto-reload configuration:
_.watchers.patchwatcher = hs.pathwatcher.new(
	os.getenv("HOME") .. "/.hammerspoon/",
	function(files)
		_.utils.reload_config(files, _.watchers)
	end
):start()
hs.alert.show("Config loaded")
