local table_merge = require("utils.table_merge").table_merge

-- Configuration segments:
local defaults = require("config.base")
local fonts = require("config.fonts")
local keys = require("config.keys")
local colors = require("wezterm_colors")

return table_merge(defaults, fonts, keys, colors)