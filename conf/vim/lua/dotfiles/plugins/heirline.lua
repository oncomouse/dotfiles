return {
	"rebelot/heirline.nvim",
	dependencies = {
		"oncomouse/czs.nvim",
	},
	event = "VeryLazy",
	config = function()
		-- We don't load LuaSnip until InsertEnter, so we don't check for it until it could possibly be loaded.
		local luasnip_might_have_loaded = false
		vim.api.nvim_create_autocmd("InsertEnter", {
			group = "dotfiles-settings",
			once = true,
			callback = function()
				luasnip_might_have_loaded = true
			end,
		})

		local heirline = require("heirline")
		local utils = require("heirline.utils")
		local conditions = require("heirline.conditions")

		-- Logical function combinators:
		conditions.fn_and = function(...)
			local fns = { ... }
			return function(...)
				for _, fn in pairs(fns) do
					if not fn(...) then
						return false
					end
				end
				return true
			end
		end
		conditions.fn_or = function(...)
			local fns = { ... }
			return function(...)
				for _, fn in pairs(fns) do
					if fn(...) then
						return true
					end
				end
				return false
			end
		end

		conditions.hide_with_fewer_columns = function(columns)
			return function()
				return vim.api.nvim_win_get_width(0) > columns
			end
		end

		local catppuccin_gui_to_cterm = {
			rosewater = { cterm = 230 },
			flamingo = { cterm = 224 },
			pink = { cterm = 5 },
			mauve = { cterm = 131 },
			red = { cterm = 1 },
			maroon = { cterm = 217 },
			peach = { cterm = 216 },
			yellow = { cterm = 3 },
			green = { cterm = 2 },
			teal = { cterm = 6 },
			sky = { cterm = 39 },
			sapphire = { cterm = 81 },
			blue = { cterm = 4 },
			lavender = { cterm = 1 },

			text = { cterm = 15 },
			subtext1 = { cterm = 250 },
			subtext0 = { cterm = 246 },
			overlay2 = { cterm = 243 },
			overlay1 = { cterm = 241 },
			overlay0 = { cterm = 240 },
			surface2 = { cterm = 239 },
			surface1 = { cterm = 238 },
			surface0 = { cterm = 237 },

			base = { cterm = 0 },
			mantle = { cterm = 233 },
			crust = { cterm = 232 },
		}
		local function setup_colors()
			local catppuccin_colors = require("catppuccin.palettes").get_palette()
			for name, gui in pairs(catppuccin_colors) do
				catppuccin_colors[name] = {
					gui = gui,
					cterm = catppuccin_gui_to_cterm[name].cterm,
				}
			end
			return vim.tbl_extend("force", catppuccin_colors, {
				inactive = {
					text = catppuccin_colors.surface0,
					base = catppuccin_colors.base,
				},
				error = catppuccin_colors.red,
				warn = catppuccin_colors.yellow,
				info = catppuccin_colors.blue,
				hint = catppuccin_colors.rosewater,
			})
		end

		local colors = setup_colors()

		local Space = { provider = " " }

		local HighlightProvider = function(opts)
			opts = vim.tbl_extend("keep", opts, {
				fg = nil,
				bg = nil,
				sp = nil,
				bold = false,
				underline = false,
			})
			return {
				hl = function()
					if conditions.is_active() then
						return opts
					end
					return {}
				end,
			}
		end
		local LuaSnipHighlight = HighlightProvider({
			bg = colors.sky.gui,
			ctermbg = colors.sky.cterm,
			fg = colors.surface0.gui,
			ctermfg = colors.surface0.cterm,
		})
		local SearchHighlight = HighlightProvider({ fg = colors.rosewater.gui, ctermfg = colors.rosewater.cterm })
		local WordCountHighlight = HighlightProvider({ fg = colors.yellow.gui, ctermfg = colors.yellow.cterm })
		local MetadataHighlight = HighlightProvider({ fg = colors.surface2.gui, ctermfg = colors.surface2.cterm })
		local MacroHighlight = HighlightProvider({
			bg = colors.flamingo.gui,
			ctermbg = colors.flamingo.cterm,
			fg = colors.surface0.gui,
			ctermfg = colors.surface0.cterm,
		})
		local PositionHighlight = {
			Column = HighlightProvider({ fg = colors.sapphire.gui, ctermfg = colors.sapphire.cterm }),
			Row = HighlightProvider({ fg = colors.mauve.gui, ctermfg = colors.mauve.cterm }),
		}
		local PercentageHighlight = HighlightProvider({ fg = colors.blue.gui, ctermfg = colors.blue.cterm })
		local ShowCmdHighlight = HighlightProvider({ fg = colors.flamingo.gui, ctermfg = colors.flamingo.cterm })

		local ViMode = {
			-- get vim current mode, this information will be required by the provider
			-- and the highlight functions, so we compute it only once per component
			-- evaluation and store it as a component attribute
			init = function(self)
				self.mode = vim.fn.mode(1) -- :h mode()

				if not self.once then
					vim.api.nvim_create_autocmd("ModeChanged", {
						pattern = "*:*o",
						command = "redrawstatus",
					})
					self.once = true
				end
			end,
			-- Now we define some dictionaries to map the output of mode() to the
			-- corresponding string and color. We can put these into `static` to compute
			-- them at initialisation time.
			static = {
				mode_colors = {
					n = colors.subtext0,
					i = colors.green,
					v = colors.sapphire,
					V = colors.sapphire,
					["\22"] = colors.sapphire,
					c = colors.orange,
					s = colors.yellow,
					S = colors.yellow,
					["\19"] = colors.yellow,
					R = colors.peach,
					r = colors.peach,
					["!"] = colors.red,
					t = colors.mauve,
				},
			},
			hl = function(self)
				local mode = self.mode:sub(1, 1)
				local color = self.mode_colors[mode] or colors.blue
				return {
					bg = color.gui,
					ctermbg = color.cterm,
					fg = colors.surface0.gui,
					ctermfg = colors.surface0.cterm,
				}
			end,
			update = {
				"ModeChanged",
			},
		}

		local WordCount = utils.insert(MetadataHighlight, {
			condition = function()
				return conditions.buffer_matches({
					filetype = {
						"markdown",
						"txt",
						"vimwiki",
					},
				})
			end,
			Space,
			{
				utils.insert(WordCountHighlight, {
					provider = function()
						return vim.fn.wordcount().words
					end,
				}),
				{ provider = "W" },
				Space,
			},
		})

		local DiagnosticComponent = function(type)
			return {
				condition = function(self)
					return self[type] > 0
				end,
				provider = function(self)
					return self.symbols[type] .. self[type]
				end,
				hl = {
					ctermbg = colors[type].cterm,
					bg = colors[type].gui,
					fg = colors.base.gui,
					ctermfg = colors.base.cterm,
				},
			}
		end

		local Diagnostics = {
			condition = conditions.has_diagnostics,
			static = {
				symbols = {
					error = vim.fn.sign_getdefined("DiagnosticSignError")[1].text,
					warn = vim.fn.sign_getdefined("DiagnosticSignWarn")[1].text,
					info = vim.fn.sign_getdefined("DiagnosticSignInfo")[1].text,
					hint = vim.fn.sign_getdefined("DiagnosticSignHint")[1].text,
				},
			},
			update = { "DiagnosticChanged", "BufEnter" },
			init = function(self)
				self.error = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.ERROR })
				self.warn = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.WARN })
				self.info = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.INFO })
				self.hint = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.HINT })
			end,
			{
				Space,
				DiagnosticComponent("error"),
				DiagnosticComponent("warn"),
				DiagnosticComponent("info"),
				DiagnosticComponent("hint"),
			},
		}

		local FileNameBlockComponent = {
			init = function(self)
				self.filename = vim.api.nvim_buf_get_name(0)
			end,
		}

		local FileNameTerminal = {
			condition = function()
				return conditions.buffer_matches({
					buftype = { "terminal" },
				})
			end,
			provider = function(self)
				return " " .. self.filename:gsub(".*:", "")
			end,
		}
		local FileNameHelp = {
			condition = function()
				return conditions.buffer_matches({
					buftype = { "help" },
				})
			end,
			provider = function(self)
				return vim.fn.fnamemodify(self.filename, ":t:r")
			end,
		}
		local FileNameQF = {
			condition = function()
				return conditions.buffer_matches({
					buftype = { "quickfix" },
				})
			end,
			{
				utils.surround({ "[", "]" }, nil, {
					provider = function()
						local name = vim.fn.getwininfo(vim.fn.win_getid())[1].loclist == 1 and "Location List"
							or "Quickfix List"
						return name
					end,
				}),
				{
					condition = function(self)
						self.title = vim.w.quickfix_title
						return self.title ~= nil
					end,
					Space,
					{
						provider = function(self)
							return self.title
						end,
					},
				},
			},
		}

		local FileNameFromFiletype = {
			condition = conditions.fn_and(function()
				return conditions.buffer_matches({
					filetype = { "packer", "^gina.*", "diff", "fugitive", "^git.*", "^$" },
				})
			end, function()
				return vim.bo.filetype ~= ""
			end),
			provider = function()
				local ft = vim.bo.filetype
				return vim.fn.join(
					vim.tbl_map(function(x)
						return x:sub(1, 1):upper() .. x:sub(2)
					end, vim.fn.split(ft, "[. -]")),
					" "
				)
			end,
		}

		local FileNameNoFileName = {
			condition = conditions.fn_and(function()
				return vim.api.nvim_buf_get_name(0) == ""
			end, function()
				return not conditions.buffer_matches({
					buftype = { "quickfix" },
				})
			end),
			provider = "[scratch]",
		}

		local FileName = {
			fallthrough = false,
			FileNameNoFileName,
			{ -- Display just the tail of the filename when status is inactive
				condition = function()
					return not conditions.is_active()
				end,
				provider = function(self)
					return vim.fn.fnamemodify(self.filename, ":t")
				end,
			},
			FileNameFromFiletype,
			FileNameTerminal,
			FileNameQF,
			FileNameHelp,
			{
				provider = function(self)
					local filename = vim.fn.fnamemodify(self.filename, ":~:.")
					if not conditions.width_percent_below(#filename, 0.4) then
						filename = vim.fn.pathshorten(filename)
					end
					if not conditions.width_percent_below(#filename, 0.4) then
						filename = vim.fn.fnamemodify(filename, ":t")
					end
					return filename
				end,
			},
		}

		local FileFlags = {
			fallthrough = false,
			condition = function()
				return not conditions.buffer_matches({
					buftype = { "help", "quickfix", "terminal" },
					filetype = { "packer", "^gina.*", "diff", "fugitive", "^git.*", "^$" },
				})
			end,
			{
				{
					condition = function()
						return vim.bo.modified
					end,
					provider = "[+]",
				},
				{
					condition = function()
						return not vim.bo.modifiable or vim.bo.readonly
					end,
					provider = "[]",
				},
			},
		}

		local FileNameBlock = utils.insert(FileNameBlockComponent, FileName, FileFlags)

		local FileIcon = {
			init = function(self)
				if self.di_ok then
					local filename = vim.api.nvim_buf_get_name(0)
					local extension = vim.fn.fnamemodify(filename, ":e")
					local icon = self.di.get_icon_color(filename, extension, { default = true })
					self.icon = icon
				end
			end,
			condition = conditions.fn_and(conditions.hide_with_fewer_columns(45), function(self)
				self.di_ok, self.di = pcall(require, "nvim-web-devicons")
				return self.di_ok
			end, function()
				return not conditions.buffer_matches({
					buftype = {
						"quickfix",
						"terminal",
					},
				})
			end),
			{
				provider = function(self)
					if vim.bo.filetype == "help" then
						return ""
					end
					return self.icon
				end,
			},
			Space,
		}

		local CmdHeightZero = {
			condition = function()
				return vim.opt.cmdheight:get() == 0
			end,
		}

		local Macro = utils.insert(
			CmdHeightZero,
			utils.insert(MacroHighlight, {
				condition = function(self)
					self.macro = vim.fn.reg_recording()
					return self.macro ~= ""
				end,
				{
					Space,
					{
						provider = function(self)
							return "@" .. self.macro
						end,
					},
					Space,
				},
			})
		)

		local ShowCmd = utils.insert(
			CmdHeightZero,
			utils.insert(ShowCmdHighlight, {
				{
					provider = "%S ",
				},
			})
		)

		local FileType = utils.insert(MetadataHighlight, {
			condition = conditions.fn_and(function()
				return not conditions.buffer_matches({
					filetype = { "packer", "^gina.*", "diff", "fugitive", "^git.*", "^$" },
					buftype = { "terminal", "quickfix", "help" },
				})
			end, conditions.hide_with_fewer_columns(45)),
			init = function(self)
				self.filetype = vim.bo.filetype
			end,
			{
				provider = function(self)
					return " " .. self.filetype
				end,
			},
		})

		local LuaSnip = {
			condition = function(self)
				if luasnip_might_have_loaded then
					self.has_luasnip, self.ls = pcall(require, "luasnip")
					return self.has_luasnip and self.ls.expand_or_locally_jumpable()
				end
				return false
			end,
			{
				condition = function(self)
					if vim.fn.mode() ~= "i" then
						return false
					end
					self.forward = self.ls.jumpable(1) and "→" or ""
					self.backward = self.ls.jumpable(-1) and "←" or ""
					self.choice = self.ls.choice_active() and "?" or ""
					return #self.forward + #self.backward + #self.choice > 0
				end,
				utils.insert(LuaSnipHighlight, {
					Space,
					{
						provider = function(self)
							return " " .. self.backward .. self.forward .. self.choice
						end,
					},
					Space,
				}),
			},
		}

		local Align = {
			{
				provider = "%=",
				hl = {
					ctermbg = colors.base.cterm,
					ctermfg = colors.base.cterm,
					bg = colors.base.gui,
					fg = colors.base.gui,
				},
			},
		}

		local Position = utils.insert(MetadataHighlight, {
			utils.insert(PositionHighlight.Row, { provider = "%l" }),
			{ provider = ":" },
			utils.insert(PositionHighlight.Column, { provider = "%c" }),
		})

		local Percentage = utils.insert(MetadataHighlight, {
			condition = conditions.hide_with_fewer_columns(45),
			Space,
			utils.surround({ "(", ")" }, nil, {
				utils.insert(PercentageHighlight, { provider = "%p%%" }),
				{ provider = " %LL" },
			}),
		})

		local Search = utils.insert(CmdHeightZero, {
			condition = function()
				return require("czs").display_results()
			end,
			init = function(self)
				self.target, self.current, self.total = require("czs").output()
			end,
			utils.insert(SearchHighlight, {
				Space,
				{ provider = "/" },
				{
					condition = conditions.hide_with_fewer_columns(75),
					{
						provider = function(self)
							return vim.fn.printf("%s", self.target)
						end,
					},
				},
				utils.surround({ "[", "]" }, nil, {
					{
						provider = function(self)
							return self.current
						end,
					},
					{ provider = "/" },
					{
						provider = function(self)
							return self.total
						end,
					},
				}),
				Space,
			}),
			{
				condition = conditions.hide_with_fewer_columns(45),
				Space,
			},
		})

		local StatuslineNC = {
			condition = function()
				return not conditions.is_active()
			end,
			FileNameBlock,
		}

		local Statusline = {
			{
				utils.insert(ViMode, { { provider = " " }, FileIcon, FileNameBlock, { provider = " " } }),
			},
			LuaSnip,
			Macro,
			Align,
			{
				condition = function()
					return not conditions.buffer_matches({
						buftype = { "terminal" },
					})
				end,
				Search,
				ShowCmd,
				WordCount,
				Position,
				Percentage,
				FileType,
				Diagnostics,
			},
			hl = {
				bg = colors.base.gui,
				ctermbg = colors.base.cterm,
			},
		}

		local StatusLines = {
			hl = function()
				if conditions.is_active() then
					return "StatusLine"
				else
					return "StatusLineNC"
				end
			end,

			fallthrough = false,

			StatuslineNC,
			Statusline,
		}

		heirline.setup({ statusline = StatusLines })

		local stl_augroup = vim.api.nvim_create_augroup("Heirline", { clear = true })
		vim.api.nvim_create_autocmd("ColorScheme", {
			callback = function()
				utils.on_colorscheme(setup_colors())
			end,
			group = stl_augroup,
		})
	end,
}
