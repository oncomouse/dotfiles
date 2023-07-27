return {
	{ "echasnovski/mini.colors", lazy = true },
	{
		"catppuccin/nvim",
		name = "catppuccin",
		build = function()
			vim.cmd([[CatppuccinBuild]])
		end,
		lazy = true,
		opts = {
			transparent_background = true,
			integrations = {
				mini = true,
				notify = true,
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
					MiniStatuslineModeNormal = {
						bg = colors.subtext0,
						fg = colors.base,
						style = {},
					},
					MiniStatuslineModeInsert = {
						bg = colors.green,
						fg = colors.base,
						style = {},
					},
					MiniStatuslineModeVisual = {
						bg = colors.sapphire,
						fg = colors.base,
						style = {},
					},
					MiniStatuslineModeReplace = { style = {} },
					MiniStatuslineModeCommand = { style = {} },
					MiniStatuslineModeOther = {
						bg = colors.mauve,
						fg = colors.base,
						style = {},
					},
					MiniStatuslineLocationRow = {
						fg = colors.mauve,
					},
					MiniStatuslineLocationColumn = {
						fg = colors.sapphire,
					},
					MiniStatuslineLocationPercentage = {
						fg = colors.blue,
					},
					MiniStatuslineDiagnosticError = {
						bg = colors.red,
						fg = colors.base,
					},
					MiniStatuslineDiagnosticWarn = {
						bg = colors.yellow,
						fg = colors.base,
					},
					MiniStatuslineDiagnosticInfo = {
						bg = colors.blue,
						fg = colors.base,
					},
					MiniStatuslineDiagnosticHint = {
						bg = colors.rosewater,
						fg = colors.base,
					},
					MiniStatuslineMacro = {
						bg = colors.flamingo,
						fg = colors.surface0,
					},
					MiniStatuslineLuaSnip = {
						bg = colors.sky,
						fg = colors.surface0,
					},
					MiniStatuslineWordcount = {
						fg = colors.yellow,
					},
					MiniTablineCurrent = {
						fg = colors.subtext0,
						style = {},
					},
					MiniTablineVisible = {
						fg = colors.surface1,
					},
					MiniTablineHidden = {
						fg = colors.surface1,
					},
					MiniTablineModifiedCurrent = {
						fg = colors.subtext0,
						style = {
							"bold",
						},
					},
					MiniTablineModifiedVisible = {
						fg = colors.surface1,
						style = {
							"bold",
						},
					},
					MiniTablineModifiedHidden = {
						fg = colors.surface1,
						style = {
							"bold",
						},
					},
					NotifyBackground = {
						bg = colors.base,
					},
					TreesitterContext = {
						bg = colors.surface0,
						style = {
							"bold",
						},
					},
					gitCommitOverflow = { fg = colors.red },
					gitCommitSummary = { fg = colors.green },
				}
			end,
		},
		init = function()
			vim.api.nvim_create_user_command("CatppuccinBuild", function()
				vim.notify("Updating catppuccin cterm information.", vim.log.levels.INFO, {
					title = "catppuccin-cterm.nvim",
				})
				for name, _ in pairs(require("catppuccin").flavours) do
					local colorscheme_name = string.format("catppuccin-%s", name)
					require("mini.colors").get_colorscheme(colorscheme_name):add_cterm_attributes():write({
						name = colorscheme_name,
					})
				end
			end, {
				force = true,
			})
		end,
	},
}
