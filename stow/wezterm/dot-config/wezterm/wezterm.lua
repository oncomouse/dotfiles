local addtional_path_prefix = os.getenv("HOME") .. "/dotfiles/conf/wezterm/"
local addtional_path = ";" .. addtional_path_prefix .. "?/init.lua;" .. addtional_path_prefix .. "?.lua"
package.path = package.path .. addtional_path

local table_merge = require("utils.table_merge").table_merge

local HOME = os.getenv("HOME")
local XDG_CACHE_HOME = os.getenv("XDG_CACHE_HOME") or HOME .. "/.cache"
local XDG_CONFIG_HOME = os.getenv("XDG_CONFIG_HOME") or HOME .. "/.config"
local DOTFILES_TARGET = os.getenv("DOTFILES_TARGET") or "laptop"

local function get_fonts()
	local ok, config = pcall(require, "fonts." .. DOTFILES_TARGET)
	if ok then
		return config
	end
	return require("fonts.laptop")
end

-- Configuration segments:
local defaults = {
	-- Color scheme:
	color_scheme_dirs = {
		XDG_CACHE_HOME .. "/wal",
	},
	color_scheme = "wal",
	hide_tab_bar_if_only_one_tab = true,
}
local fonts = get_fonts()
local keys = require("config.keys")

return table_merge(defaults, fonts, keys)
