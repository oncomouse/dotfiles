local ok, heirline = pcall(require, "heirline")
if vim.opt.termguicolors:get() and ok then
	-- No default commandline
	-- vim.opt.cmdheight = 0

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

	local function setup_colors()
		return {
			fg = utils.get_highlight("Winbar").fg,
			bg = utils.get_highlight("DiffAdd").bg,
			inactive = {
				fg = utils.get_highlight("WinbarNC").fg,
				bg = utils.get_highlight("DiffAdd").bg,
			},
			black = utils.get_highlight("Normal").bg,
			yellow = utils.get_highlight("Label").fg,
			green = utils.get_highlight("ModeMsg").fg,
			orange = utils.get_highlight("IncSearch").bg,
			cyan = utils.get_highlight("Identifier").fg,
			error = utils.get_highlight("DiagnosticError").fg,
			warn = utils.get_highlight("DiagnosticWarn").fg,
			info = utils.get_highlight("DiagnosticInfo").fg,
			hint = utils.get_highlight("DiagnosticHint").fg,
		}
	end

	local colors = setup_colors()
	colors.bg = colors.black
	colors.inactive.bg = colors.black

	-- Override Default Statusline colors for fancy rounding effects
	vim.api.nvim_set_hl(0, "Statusline", {
		fg = colors.fg,
		bg = colors.black,
	})
	vim.api.nvim_set_hl(0, "StatuslineNC", {
		fg = colors.inactive.fg,
		bg = colors.black,
	})
	vim.api.nvim_set_hl(0, "StatuslineTerm", {
		fg = colors.fg,
		bg = colors.black,
	})
	vim.api.nvim_set_hl(0, "StatuslineTermNC", {
		fg = colors.inactive.fg,
		bg = colors.black,
	})

	local Space = { provider = " " }

	-- Calling any delimiter with (false) will turn off the active condition
	local __accessor = {
		__call = function(t, ...)
			local args = { ... }
			if args[1] == false then
				return {
					provider = t.provider,
					hl = t.hl,
				}
			end
			return t
		end,
	}
	local Delimiter = {
		left = setmetatable({
			condition = conditions.is_active,
			provider = "",
			hl = {
				fg = colors.bg,
				bg = colors.black,
			},
		}, __accessor),
		right = setmetatable({
			provider = "",
			condition = conditions.is_active,
			hl = {
				fg = colors.bg,
				bg = colors.black,
			},
		}, __accessor),
	}

	local HighlightProvider = function(color)
		return {
			hl = function()
				if conditions.is_active() then
					return {
						fg = color,
					}
				end
				return {}
			end,
		}
	end
	local Highlight = HighlightProvider(colors.yellow)
	local LuaSnipHighlight = HighlightProvider(colors.cyan)
	local SearchHighlight = HighlightProvider(colors.fg)

	local WordCount = {
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
			provider = function()
				return "W:" .. vim.fn.wordcount().words
			end,
		},
	}

	local DiagnosticComponent = function(type)
		return {
			condition = function(self)
				return self[type] > 0
			end,
			provider = function(self)
				return self.symbols[type] .. self[type]
			end,
			hl = { bg = colors[type], fg = colors.black },
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
			return vim.fn.fnamemodify(self.filename, ":t")
		end,
	}
	local FileNameQF = {
		condition = function()
			return conditions.buffer_matches({
				buftype = { "quickfix" },
			})
		end,
		{
			utils.surround(
				{ "[", "]" },
				nil,
				utils.insert(Highlight, {
					provider = function()
						local name = vim.fn.getwininfo(vim.fn.win_getid())[1].loclist == 1 and "Location List"
							or "Quickfix List"
						return name
					end,
				})
			),
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
		condition = function()
			return vim.bo.filetype ~= ""
		end,
		utils.surround(
			{ "[", "]" },
			nil,
			utils.insert(Highlight, {
				provider = function()
					local ft = vim.bo.filetype
					return vim.fn.toupper(ft:sub(1, 1)) .. ft:sub(2)
				end,
			})
		),
	}

	local FileName = {
		fallthrough = false,
		FileNameHelp,
		{
			provider = function(self)
				local filename = vim.fn.fnamemodify(self.filename, ":.")
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
				buftype = { "help" },
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
				local icon, icon_color = self.di.get_icon_color(filename, extension, { default = true })
				self.icon = icon
				self.fg = icon_color
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
				return self.icon
			end,
			hl = function(self)
				return {
					fg = self.fg,
				}
			end,
		},
		Space,
	}

	local __m = {
		["\22"] = "^V",
		["\22s"] = "^V",
		["\19"] = "^S",
	}
	local modes = setmetatable({}, {
		__index = function(t, idx)
			return __m[idx] or idx
		end,
	})
	local Mode = {
		init = function(self)
			if not self.once then
				vim.api.nvim_create_autocmd("ModeChanged", {
					pattern = "*:*o",
					command = "redrawstatus",
				})
				self.once = true
			end
		end,
		condition = function(self)
			self.mode = modes[vim.fn.mode(1)]
			return not self.mode:match("^n$")
		end,
		Space,
		utils.surround({ "[", "]" }, nil, {
			provider = function(self)
				return self.mode
			end,
			hl = {
				fg = colors.green,
			},
		}),
		update = {
			"ModeChanged",
		},
	}

	local Macro = {
		condition = function(self)
			self.macro = vim.fn.reg_recording()
			return self.macro ~= ""
		end,
		Space,
		{
			provider = function(self)
				return "辶" .. self.macro
			end,
			hl = {
				fg = colors.green,
			},
		},
	}

	local CmdHeightZero = {
		condition = function()
			return vim.opt.cmdheight:get() == 0
		end,
		Mode,
		Macro,
	}

	local FileType = {
		condition = conditions.hide_with_fewer_columns(45),
		init = function(self)
			self.filetype = vim.bo.filetype
		end,
		{
			utils.surround(
				{ "[", "]" },
				nil,
				utils.insert(Highlight, {
					provider = function(self)
						return self.filetype
					end,
				})
			),
		},
	}

	local LuaSnip = {
		condition = function(self)
			self.has_luasnip, self.ls = pcall(require, "luasnip")
			return self.has_luasnip and self.ls.expand_or_locally_jumpable()
		end,
		{
			condition = function(self)
				self.forward = self.ls.jumpable(1) and "→" or ""
				self.backward = self.ls.jumpable(-1) and "←" or ""
				self.choice = self.ls.choice_active() and "?" or ""
				return #self.forward + #self.backward + #self.choice > 0
			end,
			{
				utils.surround(
					{ "[", "]" },
					nil,
					utils.insert(LuaSnipHighlight, {
						provider = function(self)
							return " " .. self.backward .. self.forward .. self.choice
						end,
					})
				),
				Space,
			},
		},
	}

	local Align = {
		Delimiter.right,
		{
			provider = "%=",
			hl = {
				bg = colors.black,
				fg = colors.black,
			},
		},
		Delimiter.left,
	}

	local Position = {
		{ provider = "%l:%c" },
	}

	local Percentage = {
		condition = conditions.hide_with_fewer_columns(45),
		Space,
		{ provider = "%p%%" },
	}

	local Search = {
		condition = function(self)
			self.searchcount = vim.fn.searchcount({ recompute = 1 })
			return vim.opt.cmdheight:get() == 0 and vim.fn.empty(self.searchcount) == 0 and self.searchcount.total ~= 0
		end,
		init = function(self)
			self.target = vim.fn.getreg("/")
			if self.searchcount.incomplete == 1 then -- Timed out
				self.current = "?"
				self.total = "??"
			elseif self.searchcount.incomplete == 2 then -- Max count exceed
				if
					self.searchcount.total > self.searchcount.maxcount
					and self.searchcount.current > self.searchcount.maxcount
				then
					self.current = vim.fn.printf(">%d", self.searchcount.current)
					self.total = vim.fn.printf(">%d", self.searchcount.total)
				elseif self.searchcount.total > self.searchcount.maxcount then
					self.current = vim.fn.printf("%d", self.searchcount.current)
					self.total = vim.fn.printf(">%d", self.searchcount.total)
				end
			else
				self.current = vim.fn.printf("%d", self.searchcount.current)
				self.total = vim.fn.printf("%d", self.searchcount.total)
			end
		end,
		{
			{ provider = "/" },
			{
				condition = conditions.hide_with_fewer_columns(75),
				utils.insert(SearchHighlight, {
					provider = function(self)
						return vim.fn.printf("%s", self.target)
					end,
				}),
			},
			utils.surround({ "[", "]" }, nil, {
				utils.insert(SearchHighlight, {
					provider = function(self)
						return self.current
					end,
				}),
				{ provider = "/" },
				utils.insert(SearchHighlight, {
					provider = function(self)
						return self.total
					end,
				}),
			}),
		},
		{
			condition = conditions.hide_with_fewer_columns(45),
			Space,
		},
	}

	local StatuslineNC = {
		condition = function()
			return not conditions.is_active()
		end,
		Delimiter.left(false),
		FileNameBlock,
		Delimiter.right(false),
		hl = {
			bg = colors.bg,
		},
	}

	local Statusline = {
		Delimiter.left,
		LuaSnip,
		FileIcon,
		FileNameBlock,
		CmdHeightZero,
		Align,
		Search,
		FileType,
		WordCount,
		Space,
		Position,
		Percentage,
		Diagnostics,
		Delimiter.right,
		hl = {
			bg = colors.bg,
		},
	}

	local SpecialFileName = {
		fallthrough = false,
		FileNameTerminal,
		FileNameQF,
		FileNameFromFiletype,
		hl = {
			bg = colors.bg,
		},
	}

	local SpecialFileNameBlock = utils.insert(FileNameBlockComponent, SpecialFileName)

	local StatuslineSpecial = {
		condition = function()
			return conditions.buffer_matches({
				buftype = { "quickfix", "terminal" },
				filetype = { "packer", "^gina.*", "diff", "fugitive", "^git.*", "^$" },
			})
		end,
		hl = {
			bg = colors.bg,
		},
		{
			Delimiter.left,
			SpecialFileNameBlock,
			CmdHeightZero,
			{
				condition = function()
					return not conditions.buffer_matches({
						buftype = { "terminal" },
					})
				end,
				Align,
				Position,
				Percentage,
			},
			Delimiter.right,
		},
	}

	local StatusLineNoFileName = {
		condition = conditions.fn_and(function()
			return vim.api.nvim_buf_get_name(0) == ""
		end, function()
			return not conditions.buffer_matches({
				buftype = { "quickfix" },
			})
		end),
		provider = "[New File]",
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

		StatusLineNoFileName,
		StatuslineNC,
		StatuslineSpecial,
		Statusline,
	}

	heirline.setup(StatusLines)

	local lushwal_available, lushwal = pcall(require, "lushwal")
	if lushwal_available then
		lushwal.add_reload_hook(function()
			heirline.reset_highlights()
			heirline.load_colors(setup_colors())
		end)
	end

	local stl_augroup = vim.api.nvim_create_augroup("Heirline", { clear = true })
	vim.api.nvim_create_autocmd("ColorScheme", {
		callback = function()
			heirline.reset_highlights()
			heirline.load_colors(setup_colors())
		end,
		group = stl_augroup,
	})
end
