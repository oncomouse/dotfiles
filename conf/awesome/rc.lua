-- luacheck: globals awesome client io tag screen
pcall(require, "luarocks.loader")
-- Update package.path
local addtional_path_prefix = os.getenv("HOME") .. "/dotfiles/conf/awesome/"
local addtional_path = ";" .. addtional_path_prefix .. "?/init.lua;" .. addtional_path_prefix .. "?.lua"
package.path = package.path .. addtional_path
local awful = require("awful")
local beautiful = require("beautiful")
local naughty = require("naughty")
require("awful.autofocus")
require("appearance")
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
naughty.connect_signal("request::display_error", function(message, startup)
	naughty.notification({
		urgency = "critical",
		title = "Oops, an error happened" .. (startup and " during startup!" or "!"),
		message = message,
	})
end)
-- Attach default layouts
tag.connect_signal("request::default_layouts", function()
	awful.layout.append_default_layouts(beautiful.default_layouts)
end)
require("smartborders")
require("bar")
require("keybindings")
require("mousebindings")
require("rules")
-- vim: foldlevel=0:foldmethod=marker
