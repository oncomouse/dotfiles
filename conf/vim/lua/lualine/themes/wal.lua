local get_wal_theme = require('dotfiles.utils.wal')
local json_scheme = get_wal_theme()

local wal = {}

local colors = {
	black = json_scheme.colors.color0,
	white = json_scheme.colors.color15,
	red = json_scheme.colors.color5,
	orange = json_scheme.colors.color1,
	purple = '#CC9CCF',
	green = json_scheme.colors.color2,
	blue = json_scheme.colors.color4,
	cyan = json_scheme.colors.color6,
	yellow = json_scheme.colors.color3,
	gray = json_scheme.colors.color7,
	darkgray = json_scheme.colors.color8,
	lightgray = json_scheme.colors.color7
}

wal.normal = {
	-- gui parameter is optional and behaves the same way as in vim's highlight command
	a = {bg = colors.blue, fg = colors.black, gui = 'bold'},
	b = {bg = colors.darkgray, fg = colors.lightgray},
	c = {bg = colors.black, fg = colors.lightgray}
}

wal.insert = {
	a = {bg = colors.green, fg = colors.black, gui = 'bold'},
	b = {bg = colors.darkgray, fg = colors.lightgray},
	c = {bg = colors.black, fg = colors.lightgray}
}

wal.visual = {
	a = {bg = colors.orange, fg = colors.black, gui = 'bold'},
	b = {bg = colors.darkgray, fg = colors.lightgray},
	c = {bg = colors.black, fg = colors.lightgray}
}

wal.replace = {
	a = {bg = colors.red, fg = colors.black, gui = 'bold'},
	b = {bg = colors.darkgray, fg = colors.lightgray},
	c = {bg = colors.black, fg = colors.lightgray}
}

wal.command = {
	a = {bg = colors.cyan, fg = colors.black, gui = 'bold'},
	b = {bg = colors.darkgray, fg = colors.lightgray},
	c = {bg = colors.black, fg = colors.lightgray}
}

-- you can assign one colorscheme to another, if a colorscheme is
-- undefined it falls back to normal
wal.terminal = {
	a = {bg = colors.purple, fg = colors.black, gui = 'bold'},
	b = {bg = colors.darkgray, fg = colors.lightgray},
	c = {bg = colors.black, fg = colors.lightgray}
}

wal.inactive = {
	a = {bg = colors.black, fg = colors.lightgray, gui = 'bold'},
	b = {bg = colors.black, fg = colors.lightgray},
	c = {bg = colors.black, fg = colors.lightgray}
}

-- lualine.theme = wal
return wal
