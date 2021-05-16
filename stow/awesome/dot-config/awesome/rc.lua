local theme = "desktop"
-- Path {{{
-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")

-- Update package.path
local addtional_path_prefix = os.getenv("HOME") .. "/dotfiles/conf/awesome/"
local addtional_path =
	";" .. addtional_path_prefix .. "?/init.lua;" .. addtional_path_prefix .. "?.lua"
package.path = package.path .. addtional_path
-- }}}
local beautiful = require("beautiful")
if not beautiful.init("~/dotfiles/conf/awesome/themes/" .. theme .. ".lua") then
	beautiful.init(require("gears").filesystem.get_themes_dir() .. "xresources/theme.lua")
end
require("init")
-- vim: foldlevel=0:foldmethod=marker
