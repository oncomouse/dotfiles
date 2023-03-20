return {
	{
		"oncomouse/catppuccin.nvim",
		dev = false,
		opts = {
			transparent_background = true,
			integrations = {
				fidget = true,
				mini = true,
			},
			custom_highlights = function(colors)
				return {
					Folded = {
						fg = colors.subtext0,
						bg = colors.surface0,
					},
					MiniStatuslineFileinfo = {
						fg = colors.surface2,
						bg = colors.base,
					},
					MiniStatuslineModeNormal = { style = {} },
					MiniStatuslineModeInsert = { style = {} },
					MiniStatuslineModeVisual = { style = {} },
					MiniStatuslineModeReplace = { style = {} },
					MiniStatuslineModeCommand = { style = {} },
					MiniStatuslineModeOther = { style = {} },
					MiniStatuslineLocationRow = {
						fg = colors.mauve,
					},
					MiniStatuslineLocationColumn = {
						fg = colors.sapphire,
					},
					MiniStatuslineLocationPercentage = {
						fg = colors.blue,
					},
					MiniStatsulineDiagnosticError = {
						bg = colors.red,
						fg = colors.base,
					},
					MiniStatsulineDiagnosticWarn = {
						bg = colors.yellow,
						fg = colors.base,
					},
					MiniStatsulineDiagnosticInfo = {
						bg = colors.blue,
						fg = colors.base,
					},
					MiniStatsulineDiagnosticHint = {
						bg = colors.rosewater,
						fg = colors.base,
					},
					MiniStatuslineMacro = {
						bg = colors.flamingo,
						fg = colors.surface0,
					},
					MiniStatuslineSearch = {
						fg = colors.rosewater,
					},
					MiniStatuslineShowcmd = {
						fg = colors.flamingo,
					},
					MiniStatuslineLuaSnip = {
						bg = colors.sky,
						fg = colors.surface0,
					},
					MiniStatuslineWordcount = {
						fg = colors.yellow,
					},
				}
			end,
		},
	}, -- Theme (catppuccin with cterm support)
}
