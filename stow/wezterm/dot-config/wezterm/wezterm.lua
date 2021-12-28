-- Extend path:
local addtional_path_prefix = os.getenv("HOME") .. "/dotfiles/conf/wezterm/"
local addtional_path = ";" .. addtional_path_prefix .. "?/init.lua;" .. addtional_path_prefix .. "?.lua"
package.path = package.path .. addtional_path

return require("config")
