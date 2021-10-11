-- luacheck: globals awesome client io tag screen
pcall(require, "luarocks.loader")
-- Update package.path
local addtional_path_prefix = os.getenv("HOME") .. "/dotfiles/conf/awesome/"
local addtional_path = ";" .. addtional_path_prefix .. "?/init.lua;" .. addtional_path_prefix .. "?.lua"
package.path = package.path .. addtional_path
require("appearance")
require("keybindings")
require("mousebindings")
require("bar")
require("rules")
client.connect_signal("manage", function (c)
    if not awesome.startup then awful.client.setslave(c) end
end)
