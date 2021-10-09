-- luacheck: globals awesome client io tag screen
pcall(require, "luarocks.loader")
-- Update package.path
local addtional_path_prefix = os.getenv("HOME") .. "/dotfiles/conf/awesome/"
local addtional_path = ";" .. addtional_path_prefix .. "?/init.lua;" .. addtional_path_prefix .. "?.lua"
package.path = package.path .. addtional_path
require("appearance")
require("bar")
require("keybindings")
require("mousebindings")
require("rules")
