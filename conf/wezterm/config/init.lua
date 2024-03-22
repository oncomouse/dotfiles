local table_merge = require("utils.table_merge").table_merge

-- Configuration segments:
local defaults = require("config.base")
local fonts = require("config.fonts")
local keys = require("config.keys")
local config = {
	color_scheme = "Catppuccin Latte",
	cursor_blink_rate = 0,
}

return table_merge(defaults, fonts, keys, config)
