--luacheck: globals hs
require"hs.application"
-- _ is the universal configuration object:
local _ = {}
-- Load external libraries:
_.utils = {
	window_movements = require("utils.window_movements"),
	string_literal = require("utils.string_literal"),
	reload_config = require("utils.reload_config"),
	show_spotify_song = require("utils.show_spotify_song"),
	show_date = require("utils.show_date"),
	make_modal = require("utils.make_modal"),
	bind_generator = require("utils.bind_generator"),
	caffeine = require("utils.caffeine"),
	spaces = require("utils.spaces"),
	fuzzy_switch = require("utils.fuzzy_switch"),
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
-- Hot keys:
-- Launched using <mod>+<hotkey>
function focusWindowDirection(dir)
	return function()
		local win = hs.window.focusedWindow()
		if win and win["focusWindow" .. dir] then
			return win["focusWindow" .. dir](nil, false, true)
		end
	end
end
function control_ncspot(method)
	return function()
		local command =
			"/usr/bin/env dbus-send --print-reply --dest=org.mpris.MediaPlayer2.ncspot /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player." .. method
		hs.execute(command, true)
	end
end

_.hot_keys = {
	-- No modifiers:
	_ = {},
	-- Hammerspoon console:
	mash = { space = hs.toggleConsole },
	hyper = {
		-- Show current Spotify song:
		space = _.utils.show_spotify_song,
		-- space = control_ncspot("PlayPause"),
		-- up = control_ncspot("PlayPause"),
		-- down = control_ncspot("PlayPause"),
		-- right = control_ncspot("Next"),
		-- left = control_ncspot("Previous"),
		-- Show date:
		d = _.utils.show_date,
		-- Toggle caffeine:
		c = _.utils.caffeine,
		p = _.utils.fuzzy_switch,
	},
}
-- For some reason the hot_keys api (but not the modals one) does not work with number keys:
-- hs.hotkey.bindSpec({ _.mods.hyper, "1" }, _.utils.spaces.change_to_space(1))
-- hs.hotkey.bindSpec({ _.mods.hyper, "2" }, _.utils.spaces.change_to_space(2))
-- hs.hotkey.bindSpec({ _.mods.hyper, "3" }, _.utils.spaces.change_to_space(3))
-- hs.hotkey.bindSpec({ _.mods.hyper, "4" }, _.utils.spaces.change_to_space(4))
-- hs.hotkey.bindSpec({ _.mods.hyper, "5" }, _.utils.spaces.change_to_space(5))
-- hs.hotkey.bindSpec({ _.mods.hyper, "6" }, _.utils.spaces.change_to_space(6))
-- hs.hotkey.bindSpec({ _.mods.hyper, "7" }, _.utils.spaces.change_to_space(7))
-- hs.hotkey.bindSpec({ _.mods.hyper, "8" }, _.utils.spaces.change_to_space(8))
-- hs.hotkey.bindSpec({ _.mods.hyper, "9" }, _.utils.spaces.change_to_space(9))
-- hs.hotkey.bindSpec(
-- 	{ _.mods.hyper, "left" },
-- 	_.utils.spaces.change_to_nearby_space(-1)
-- )

-- Modal shortcuts:
-- modals are enter with <mod>+<hotkey>, then trigger by pressing the combo below.
-- <esc> in a modal cancels the sequence; the sequence also cancels after 10 seconds.
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
		-- Move window to numbered spaces:
		m = {
			[1] = _.utils.spaces.send_to_space(1),
			[2] = _.utils.spaces.send_to_space(2),
			[3] = _.utils.spaces.send_to_space(3),
			[4] = _.utils.spaces.send_to_space(4),
			[5] = _.utils.spaces.send_to_space(5),
			[6] = _.utils.spaces.send_to_space(6),
			[7] = _.utils.spaces.send_to_space(7),
			[8] = _.utils.spaces.send_to_space(8),
			[9] = _.utils.spaces.send_to_space(9),
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
		s = {
			space = control_ncspot("PlayPause"),
			up = control_ncspot("PlayPause"),
			down = control_ncspot("PlayPause"),
			right = control_ncspot("Next"),
			left = control_ncspot("Previous"),
		},
	},
}
_.watchers = {}
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
