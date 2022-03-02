pcall(require, "luarocks.loader")
-- Update package.path
local addtional_path_prefix = os.getenv("HOME") .. "/dotfiles/conf/awesome/"
local addtional_path = ";" .. addtional_path_prefix .. "?/init.lua;" .. addtional_path_prefix .. "?.lua"
package.path = package.path .. addtional_path
-- Autostart
local autostart = true
if autostart then
	require("awful").spawn(os.getenv("HOME") .. "/dotfiles/conf/awesome/autostart", false)
	autostart = false
end
require("appearance")
require("keybindings")
require("mousebindings")
require("rules")
require("signals")
require("notifications").init()
require("bar")
