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
	spaces = require("hs._asm.undocumented.spaces"),
	fuzzy_switch = require("utils.fuzzy_switch"),
}
local monitorId = ""
function createMissingSpace(space)
	local count = space - #_.spaces
	for _i = 1, count do
		_.spaces[#_.spaces + 1] = _.utils.spaces.createSpace(monitorId)
	end
	return _.spaces[space]
end
function sendToSpace(space)
	return function()
		local win = hs.window.focusedWindow()
		if (#_.spaces <= space) then
			win:spacesMoveTo(createMissingSpace(space))
		else
			win:spacesMoveTo(_.spaces[space])
		end
	end
end
function changeToSpace(space)
	return function()
		if (#_.spaces <= space) then
			createMissingSpace(space)
		end
		_.utils.spaces.changeToSpace(_.spaces[space])
	end
end
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
		-- Toggle caffeine:
		c = _.utils.caffeine,
		p = _.utils.fuzzy_switch,
		left = function()
			hs.window.focusedWindow():focusWindowWest()
		end,
		right = function()
			hs.window.focusedWindow():focusWindowEast()
		end,
		down = function()
			hs.window.focusedWindow():focusWindowSouth()
		end,
		up = function()
			hs.window.focusedWindow():focusWindowNorth()
		end,
	},
}
hs.hotkey.bindSpec({ _.mods.hyper, "1" }, changeToSpace(1))
hs.hotkey.bindSpec({ _.mods.hyper, "2" }, changeToSpace(2))
hs.hotkey.bindSpec({ _.mods.hyper, "3" }, changeToSpace(3))
hs.hotkey.bindSpec({ _.mods.hyper, "4" }, changeToSpace(4))
hs.hotkey.bindSpec({ _.mods.hyper, "5" }, changeToSpace(5))
hs.hotkey.bindSpec({ _.mods.hyper, "6" }, changeToSpace(6))
hs.hotkey.bindSpec({ _.mods.hyper, "7" }, changeToSpace(7))
hs.hotkey.bindSpec({ _.mods.hyper, "8" }, changeToSpace(8))
hs.hotkey.bindSpec({ _.mods.hyper, "9" }, changeToSpace(9))
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
		m = {
			[1] = sendToSpace(1),
			[2] = sendToSpace(2),
			[3] = sendToSpace(3),
			[4] = sendToSpace(4),
			[5] = sendToSpace(5),
			[6] = sendToSpace(6),
			[7] = sendToSpace(7),
			[8] = sendToSpace(8),
			[9] = sendToSpace(9),
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
_.generators = {
	hotkeys = _.utils.bind_generator(_.mods, hs.hotkey.bind),
	modals = _.utils.bind_generator(_.mods, _.utils.make_modal),
}

-- Window management:
hs.window.animationDuration = 0

-- Hook up shortcuts:
_.generators.hotkeys(_.hot_keys)
_.generators.modals(_.modal_keys)

function get_spaces()
	local layout = _.utils.spaces.layout()
	for k in pairs(layout) do
		monitorId = k
	end
	return layout[monitorId]
end

_.spaces = get_spaces()

-- Auto-reload configuration:
_.watchers.patchwatcher = hs.pathwatcher.new(
	os.getenv("HOME") .. "/.hammerspoon/",
	function(files)
		_.utils.reload_config(files, _.watchers)
	end
):start()
hs.alert.show("Config loaded")
