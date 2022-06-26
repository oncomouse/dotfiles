local heirline_available, heirline = pcall(require, "heirline")

if heirline_available then
	local utils = require("heirline.utils")
	local conditions = require("heirline.conditions")

	local function setup_colors()
		return {
			black = require("lushwal").colors.black.hex,
			yellow = require("lushwal").colors.yellow.hex,
			error = require("lushwal").colors.red.hex,
			warn = require("lushwal").colors.yellow.hex,
			info = require("lushwal").colors.blue.hex,
			hint = require("lushwal").colors.cyan.hex,
		}
	end

	local colors = setup_colors()

	local Space = { provider = " " }

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
		provider = function()
			return " W:" .. vim.fn.wordcount().words
		end,
	}

	local Diagnostics = {
		condition = conditions.has_diagnostics,
		static = {
			symbols = {
				error = vim.api.nvim_get_hl_by_name("DiagnosticSignError", true).text,
				warn = vim.api.nvim_get_hl_by_name("DiagnosticSignWarn", true).text,
				info = vim.api.nvim_get_hl_by_name("DiagnosticSignInfo", true).text,
				hint = vim.api.nvim_get_hl_by_name("DiagnosticSignHint", true).text,
			},
		},
		init = function(self)
			self.error = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.ERROR })
			self.warn = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.WARN })
			self.info = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.INFO })
			self.hint = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.HINT })
		end,
		{
			{
				condition = function(self)
					return self.error > 0
				end,
				provider = function(self)
					return self.symbols.error .. " " .. self.error
				end,
				hl = { bg = colors.error, fg = colors.black },
			},
			{
				condition = function(self)
					return self.warn > 0
				end,
				provider = function(self)
					return self.symbols.warn .. " " .. self.warn
				end,
				hl = { bg = colors.warn, fg = colors.black },
			},
			{
				condition = function(self)
					return self.info > 0
				end,
				provider = function(self)
					return self.symbols.info .. " " .. self.info
				end,
				hl = { bg = colors.info, fg = colors.black },
			},
			{
				condition = function(self)
					return self.hint > 0
				end,
				provider = function(self)
					return self.symbols.hint .. " " .. self.hint
				end,
				hl = { bg = colors.hint, fg = colors.black },
			},
		},
	}

	local FileNameBlock = {
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
			{ provider = "[" },
			{
				provider = function()
					local name = vim.fn.getwininfo(vim.fn.win_getid())[1].loclist == 1 and "Location List"
						or "Quickfix List"
					return name
				end,
				hl = function()
					if conditions.is_active() then
						return {
							fg = colors.yellow
						}
					end
					return {}
				end
			},
			{ provider = "]" },
			{
				provider = function()
					local title = vim.w.quickfix_title or ""
					return title
				end,
			},
		},
	}
	local FileNameGit = {
		condition = function()
			return conditions.buffer_matches({
				filetype = { "^gina.*", "diff", "fugitive", "^git.*" },
			})
		end,
		{
			{ provider = "[" },
			{
				provider = function()
					local title = "Git "
					local ft = vim.bo.filetype
					if ft == "gina-commit" then
						title = title .. "Commit"
					elseif ft == "diff" then
						title = title .. "Diff"
					end
					return title
				end,
				hl = function()
					if conditions.is_active() then
						return {
							fg = colors.yellow
						}
					end
					return {}
				end
			},
			{ provider = "]" },
		},
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
		{
			condition = function()
				return conditions.buffer_matches({
					buftype = { "quickfix", "terminal" },
				})
			end,
			provider = "",
		},
		{
			{
				condition = function()
					return vim.bo.modified
				end,
				provider = "[+]",
			},
			{
				condition = function()
					return conditions.buffer_matches({
						buftype = { "help" },
					})
				end,
				provider = "[?]",
			},
			{
				condition = function()
					return not vim.bo.modifiable or vim.bo.readonly
				end,
				provider = "[]",
			},
		},
	}

	FileNameBlock = utils.insert(FileNameBlock, FileName, FileFlags)

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
			{ provider = "[" },
			{
				provider = function(self)
					return self.filetype
				end,
				hl = function()
					if conditions.is_active() then
						return {
							fg = colors.yellow
						}
					end
					return {}
				end
			},
			{ provider = "]" },
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
				{ provider = "[ " },
				{
					provider = function(self)
						return self.backward .. self.forward .. self.choice
					end,
					hl = {
						bold = true,
					},
				},
				{ provider = "]" },
			},
		},
	}

	local Align = {
		provider = "%=",
	}

	local Position = {
		Space,
		provider = "%l:%c",
	}

	local Percentage = {
		Space,
		provider = "%p%%",
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

	local FileNameSpecial = {
		init = function(self)
			self.filename = vim.api.nvim_buf_get_name(0)
		end,
		{
			init = utils.pick_child_on_condition,
			FileNameTerminal,
			FileNameQF,
			FileNameGit,
			FileNameNoName,
		},
	}

	local StatuslineSpecial = {
		condition = function()
			return conditions.buffer_matches({
				buftype = { "quickfix", "terminal" },
				filetype = { "^gina.*", "diff", "fugitive", "^git.*", "^$" },
			})
		end,
		Space,
		FileNameSpecial,
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
		}
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

	local lushwal_available, lushwal = pcall(require, "lushwal.nvim")
	if lushwal_available and vim.g.colors_name == "lushwal" then
		lushwal.add_reload_hook(function()
			heirline.reset_highlights()
			heirline.load_colors(setup_colors())
		end)
	end

	vim.api.nvim_create_augroup("Heirline", { clear = true })
	vim.api.nvim_create_autocmd("ColorScheme", {
		callback = function()
			heirline.reset_highlights()
			heirline.load_colors(setup_colors())
		end,
		group = "Heirline",
	})
end
