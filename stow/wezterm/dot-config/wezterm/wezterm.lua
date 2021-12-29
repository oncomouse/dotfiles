-- Extend path:
local function extend_path(add_dir)
	local addtional_path_prefix = add_dir
	local addtional_path = ";" .. addtional_path_prefix .. "?/init.lua;" .. addtional_path_prefix .. "?.lua"
	package.path = package.path .. addtional_path
end

extend_path(os.getenv("HOME") .. "/dotfiles/conf/wezterm/")
extend_path(require("config.env").XDG_CACHE_HOME .. "/wal/")

return require("config")
