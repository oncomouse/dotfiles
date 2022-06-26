local heirline_available, heirline = pcall(require, "heirline")

if heirline_available then
	local lushwal_available, lushwal = pcall(require, "lushwal.nvim")
	local utils = require("heirline.utils")
	local conditions = require("heirline.conditions")

	local function setup_colors()
		if lushwal_available then
			return {
				black = lushwal.colors.black.hex,
				yellow = lushwal.colors.yellow.hex,
				cyan = lushwal.colors.cyan.hex,
				error = lushwal.colors.red.hex,
				warn = lushwal.colors.yellow.hex,
				info = lushwal.colors.blue.hex,
				hint = lushwal.colors.cyan.hex,
			}
		end
		return {
			black = utils.get_highlight("Normal").bg,
			yellow = utils.get_highlight("DiagnosticWarn").fg,
			cyan = utils.get_highlight("DiagnosticHint").fg,
			error = utils.get_highlight("DiagnosticError").fg,
			warn = utils.get_highlight("DiagnosticWarn").fg,
			info = utils.get_highlight("DiagnosticInfo").fg,
			hint = utils.get_highlight("DiagnosticHint").fg,
		}
	end

	local colors = setup_colors()

	local Space = { provider = " " }

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
				provider = function()
					local title = vim.w.quickfix_title or ""
					return title
				end,
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

	local FileNameNoName = {
		condition = function(self)
			return self.filename == ""
		end,
		provider = "",
	}

	local FileName = {
		init = utils.pick_child_on_condition,
		FileNameHelp,
		{
			provider = function(self)
				local filename = vim.fn.fnamemodify(self.filename, ":.")
				if conditions.width_percent_below(#filename, 0.25) then
					filename = vim.fn.pathshorten(filename)
				end
				return filename
			end,
		},
	}

	local FileFlags = {
		init = utils.pick_child_on_condition,
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
		condition = function(self)
			self.di_ok, self.di = pcall(require, "nvim-web-devicons")
			local buf_ignore = conditions.buffer_matches({
				buftype = {
					"quickfix",
					"terminal",
				},
			})
			return not buf_ignore and self.di_ok
		end,
		Space,
		{
			provider = function(self)
				if not self.di_ok then
					return ""
				end
				return self.icon
			end,
			hl = function(self)
				if not self.di_ok then
					return {}
				end
				return {
					fg = self.fg,
				}
			end,
		},
	}

	local FileType = {
		condition = function()
			return not conditions.buffer_matches({
				buftype = { "quickfix", "terminal" },
				filetype = { "^$" },
			})
		end,
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
				self.forward = self.ls.jumpable(1) and "" or ""
				self.backward = self.ls.jumpable(-1) and "" or ""
				self.choice = self.ls.choice_active() and "?" or ""
				return #self.forward + #self.backward + #self.choice > 0
			end,
			{
				Space,
				utils.surround(
					{ "[", "]" },
					nil,
					utils.insert(LuaSnipHighlight, {
						provider = function(self)
							return " " .. self.backward .. self.forward .. self.choice
						end,
					})
				),
			},
		},
	}

	local Align = {
		provider = "%=",
	}

	local Position = {
		Space,
		{ provider = "%l:%c" },
	}

	local Percentage = {
		Space,
		{ provider = "%p%%" },
	}

	local StatuslineNC = {
		condition = function()
			return not conditions.is_active()
		end,

		Align,
		FileNameBlock,
	}

	local Statusline = {
		LuaSnip,
		FileIcon,
		Space,
		FileNameBlock,
		Align,
		FileType,
		WordCount,
		Position,
		Percentage,
		Diagnostics,
		Space,
	}

	local SpecialFileName = {
		init = utils.pick_child_on_condition,
		FileNameTerminal,
		FileNameQF,
		FileNameFromFiletype,
		FileNameNoName,
	}

	local SpecialFileNameBlock = utils.insert(FileNameBlockComponent, SpecialFileName)

	local StatuslineSpecial = {
		condition = function()
			return conditions.buffer_matches({
				buftype = { "quickfix", "terminal" },
				filetype = { "packer", "^gina.*", "diff", "fugitive", "^git.*", "^$" },
			})
		end,
		Space,
		SpecialFileNameBlock,
		{
			condition = function()
				return not conditions.buffer_matches({
					buftype = { "terminal" },
				})
			end,
			Align,
			Position,
			Percentage,
			Space,
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

		init = utils.pick_child_on_condition,

		StatuslineSpecial,
		StatuslineNC,
		Statusline,
	}

	heirline.load_colors(setup_colors())
	heirline.setup(StatusLines)

	if lushwal_available and vim.g.colors_name == "lushwal" then
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
