-- luacheck: globals vim
local conditions = require("heirline.conditions")
local utils = require("heirline.utils")
local colors = {
	red = utils.get_highlight("AnsiColor1").fg,
	green = utils.get_highlight("AnsiColor2").fg,
	blue = utils.get_highlight("AnsiColor4").fg,
	gray = utils.get_highlight("AnsiColor7").fg,
	dark_gray = utils.get_highlight("AnsiColor8").fg,
	orange = utils.get_highlight("AnsiColor5").fg,
	purple = utils.get_highlight("Conditional").fg,
	cyan = utils.get_highlight("AnsiColor6").fg,
	yellow = utils.get_highlight("AnsiColor3").fg,
	black = utils.get_highlight("AnsiColor0").fg,
	diag = {
		warn = utils.get_highlight("DiagnosticWarn").fg,
		error = utils.get_highlight("DiagnosticError").fg,
		hint = utils.get_highlight("DiagnosticHint").fg,
		info = utils.get_highlight("DiagnosticInfo").fg,
	},
	git = {
		del = utils.get_highlight("DiffDelete").fg,
		add = utils.get_highlight("DiffAdd").fg,
		change = utils.get_highlight("DiffChange").fg,
	},
}
-- local

local mode_wrapper = function(delimiter, component)
	component = utils.clone(component)
	component.hl = component.hl or {}
	local old_hl_func, old_hl
	if type(component.hl) == "function" then
		old_hl_func = component.hl
	else
		old_hl = component.hl
		old_hl_func = function()
			return utils.clone(old_hl)
		end
	end
	component.hl = function(obj)
		local hl = old_hl_func(obj)
		local mode = obj.mode:sub(1, 1) -- get only the first mode character
		hl.bg = obj.mode_colors[mode]
		if not hl.fg or hl.fg == colors.gray then
			hl.fg = colors.dark_gray
		end
		return hl
	end
	return {
		static = {
			mode_colors = {
				n = colors.blue,
				i = colors.green,
				v = colors.orange,
				V = colors.orange,
				["^V"] = colors.orange,
				c = colors.cyan,
				s = colors.purple,
				S = colors.purple,
				["^S"] = colors.purple,
				R = colors.red,
				r = colors.red,
				["!"] = colors.red,
				t = colors.purple,
			},
		},
		init = function(self)
			self.mode = vim.fn.mode(1) -- :h mode()
		end,
		{
			hl = function(self)
				local mode = self.mode:sub(1, 1) -- get only the first mode character
				return { fg = self.mode_colors[mode], bg = colors.black }
			end,
			provider = delimiter[1],
		},
		component,
		{
			hl = function(self)
				local mode = self.mode:sub(1, 1) -- get only the first mode character
				return { fg = self.mode_colors[mode], bg = colors.dark_gray }
			end,
			provider = delimiter[2],
		},
	}
end

local ViMode = {
	-- get vim current mode, this information will be required by the provider
	-- and the highlight functions, so we compute it only once per component
	-- evaluation and store it as a component attribute
	init = function(self)
		self.mode = vim.fn.mode(1) -- :h mode()
	end,
	-- Now we define some dictionaries to map the output of mode() to the
	-- corresponding string and color. We can put these into `static` to compute
	-- them at initialisation time.
	static = {
		mode_names = { -- change the strings if yow like it vvvvverbose!
			n = "N",
			no = "N?",
			nov = "N?",
			noV = "N?",
			["no^V"] = "N?",
			niI = "Ni",
			niR = "Nr",
			niV = "Nv",
			nt = "Nt",
			v = "V",
			vs = "Vs",
			V = "V_",
			Vs = "Vs",
			["^V"] = "^V",
			["^Vs"] = "^V",
			s = "S",
			S = "S_",
			["^S"] = "^S",
			i = "I",
			ic = "Ic",
			ix = "Ix",
			R = "R",
			Rc = "Rc",
			Rx = "Rx",
			Rv = "Rv",
			Rvc = "Rv",
			Rvx = "Rv",
			c = "C",
			cv = "Ex",
			r = "...",
			rm = "M",
			["r?"] = "?",
			["!"] = "!",
			t = "T",
		},
	},
	-- We can now access the value of mode() that, by now, would have been
	-- computed by `init()` and use it to index our strings dictionary.
	-- note how `static` fields become just regular attributes once the
	-- component is instantiated.
	-- To be extra meticulous, we can also add some vim statusline syntax to
	-- control the padding and make sure our string is always at least 2
	-- characters long. Plus a nice Icon.
	provider = function(self)
		return self.mode_names[self.mode]
	end,
	-- Same goes for the highlight. Now the foreground will change according to the current mode.
	hl = function()
		return { fg = colors.dark_gray, style = "bold" }
	end,
}
local Snippets = {
	-- check that we are in insert or select mode
	condition = function()
		return vim.tbl_contains({ "s", "i" }, vim.fn.mode())
	end,
	provider = function()
		local forward = (vim.fn["vsnip#jumpable"](1) == 1) and "" or ""
		local backward = (vim.fn["vsnip#jumpable"](-1) == 1) and " " or ""
		return backward .. forward
	end,
	hl = { fg = colors.dark_gray, syle = "bold" },
}
local FileNameBlock = {
	-- let's first set up some attributes needed by this component and it's children
	init = function(self)
		self.filename = vim.api.nvim_buf_get_name(0)
	end,
}
-- We can now define some children separately and add them later

local FileIcon = {
	init = function(self)
		local filename = self.filename
		local extension = vim.fn.fnamemodify(filename, ":e")
		self.icon, self.icon_color = require("nvim-web-devicons").get_icon_color(
			filename,
			extension,
			{ default = true }
		)
	end,
	provider = function(self)
		return self.icon and (self.icon .. " ")
	end,
	hl = function(self)
		return { fg = self.icon_color }
	end,
}

local FileName = {
	provider = function(self)
		-- first, trim the pattern relative to the current directory. For other
		-- options, see :h filename-modifers
		local filename = vim.fn.fnamemodify(self.filename, ":.")
		if filename == "" then
			return "[No Name]"
		end
		-- now, if the filename would occupy more than 1/4th of the available
		-- space, we trim the file path to its initials
		if not conditions.width_percent_below(#filename, 0.25) then
			filename = vim.fn.pathshorten(filename)
		end
		return filename
	end,
	-- hl = { fg = utils.get_highlight("Directory").fg },
}

local FileFlags = {
	{
		provider = function()
			if vim.bo.modified then
				return "[+]"
			end
		end,
		-- hl = { fg = colors.green },
	},
	{
		provider = function()
			if not vim.bo.modifiable or vim.bo.readonly then
				return ""
			end
		end,
		-- hl = { fg = colors.orange },
	},
}

-- Now, let's say that we want the filename color to change if the buffer is
-- modified. Of course, we could do that directly using the FileName.hl field,
-- but we'll see how easy it is to alter existing components using a "modifier"
-- component

-- local FileNameModifer = {
-- 	hl = function()
-- 		if vim.bo.modified then
-- 			-- use `force` because we need to override the child's hl foreground
-- 			return { fg = colors.cyan, style = "bold", force = true }
-- 		end
-- 	end,
-- }

-- let's add the children to our FileNameBlock component
FileName = utils.insert(
	FileNameBlock,
	FileIcon,
	FileName,
	-- utils.insert(FileNameModifer, FileName), -- a new table where FileName is a child of FileNameModifier
	unpack(FileFlags), -- A small optimisation, since their parent does nothing
	{ provider = "%<" } -- this means that the statusline is cut here when there's not enough space
)

local Space = { provider = " " }
local FileType = {
	condition = function()
		return #vim.bo.filetype > 0
	end,
	{
		provider = "[",
	},
	{
		provider = function()
			return vim.bo.filetype
		end,
		hl = function()
			return conditions.is_active() and { fg = colors.yellow } or {}
		end,
	},
	{
		provider = "]",
	},
}

local Diagnostics = {

	condition = conditions.has_diagnostics,

	static = {
		error_icon = vim.fn.sign_getdefined("DiagnosticSignError")[1].text,
		warn_icon  = vim.fn.sign_getdefined("DiagnosticSignWarn")[1].text,
		info_icon  = vim.fn.sign_getdefined("DiagnosticSignInfo")[1].text,
		hint_icon  = vim.fn.sign_getdefined("DiagnosticSignHint")[1].text,
	},

	init = function(self)
		self.errors   = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.ERROR })
		self.warnings = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.WARN })
		self.hints    = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.HINT })
		self.info     = #vim.diagnostic.get(0, { severity = vim.diagnostic.severity.INFO })
	end,
	{
		Space,
	},
	{
		condition = function(self)
			return self.errors > 0
		end,
		{
			{
				provider = "",
				hl = { fg = colors.diag.error },
			},
			{
				provider = function(self)
					return self.errors > 0 and (" " .. self.error_icon .. self.errors .. " ")
				end,
				hl = { bg = colors.diag.error, fg = colors.dark_gray },
			},
			{
				provider = "",
				condition = function(self) return self.warnings == 0 and self.info == 0 and self.hints == 0 end,
				hl = { fg = colors.diag.error, bg = colors.black },
			},
		},
	},
	{
		condition = function(self)
			return self.warnings > 0
		end,
		{
			{
				provider = "",
				hl = function(self)
					local h = { fg = colors.diag.warn }
					if self.errors > 0 then
						h.bg = colors.diag.error
					end
					return h
				end,
			},
			{
				provider = function(self)
					return self.warnings > 0 and (" " .. self.warn_icon .. self.warnings .. " ")
				end,
				hl = { bg = colors.diag.warn, fg = colors.dark_gray },
			},
			{
				provider = "",
				condition = function(self) return self.info == 0 and self.hints == 0 end,
				hl = { fg = colors.diag.warn, bg = colors.black },
			},
		},
	},
	{
		condition = function(self)
			return self.info > 0
		end,
		{
			{
				provider = "",
				hl = function(self)
					local h = { fg = colors.diag.info }
					if self.errors > 0 then
						h.bg = colors.diag.warn
					end
					return h
				end,
			},
			{
				provider = function(self)
					return self.info > 0 and (" " .. self.info_icon .. self.info .. " ")
				end,
				hl = { bg = colors.diag.info, fg = colors.dark_gray },
			},
			{
				provider = "",
				condition = function(self) return self.hints == 0 end,
				hl = { fg = colors.diag.info, bg = colors.black },
			},
		},
	},
	{
		condition = function(self)
			return self.hints > 0
		end,
		{
			{
				provider = "",
				hl = function(self)
					local h = { fg = colors.diag.hint }
					if self.errors > 0 then
						h.bg = colors.diag.info
					end
					return h
				end,
			},
			{
				provider = function(self)
					return self.hints > 0 and (" " .. self.hint_icon .. self.hint .. " ")
				end,
				hl = { bg = colors.diag.hint, fg = colors.dark_gray },
			},
			{
				provider = "",
				hl = { fg = colors.diag.hint, bg = colors.black },
			},
		},
	},
}
local TerminalName = {
	-- we could add a condition to check that buftype == 'terminal'
	-- or we could do that later (see #conditional-statuslines below)
	provider = function()
		local tname, _ = vim.api.nvim_buf_get_name(0):gsub(".*:", "")
		return " " .. tname
	end,
	-- hl = { style = "bold" },
}

local WordCount = {
	condition = function()
		return vim.tbl_contains({
			"markdown",
			"txt",
			"vimwiki",
		}, vim.opt.filetype:get())
	end,
	init = function(self)
		self.words = vim.fn.wordcount().words
	end,
	{
		Space,
		{
			provider = function(self)
				return "W:" .. self.words
			end,
		},
	},
}

local HelpFileName = {
	condition = function()
		return vim.bo.filetype == "help"
	end,
	{
		{
			provider = "[",
		},
		{
			provider = "Help",
			hl = { fg = colors.yellow },
		},
		{
			provider = "]",
		},
		Space,
		FileIcon,
		{
			provider = function()
				local filename = vim.api.nvim_buf_get_name(0)
				return vim.fn.fnamemodify(filename, ":t")
			end,
		},
		Space,
		FileFlags,
	},
	-- hl = { fg = colors.blue },
}

local QuickfixName = {
	init = function(self)
		self.title = vim.w.quickfix_title or ""
		self.name = vim.fn.getwininfo(vim.fn.win_getid())[1].loclist == 1 and "Location List" or "Quickfix List"
	end,
	condition = function()
		return vim.bo.filetype == "qf"
	end,
	{
		{
			provider = "[",
		},
		{
			provider = function(self)
				return self.name
			end,
			hl = { fg = colors.yellow },
		},
		{
			provider = "]",
		},
		Space,
		{
			provider = function(self)
				return self.title
			end,
		},
	},
}

local Position = {
	init = function(self)
		self.line = vim.fn.line(".")
		self.col = vim.fn.col(".")
		self.last_line = vim.fn.line("$")
	end,
	{
		{
			provider = function(self)
				return string.format("%s:%s", self.line, self.col)
			end,
		},
		Space,
		{
			provider = function(self)
				return string.format("%d%%%%", self.line / self.last_line * 100)
			end,
		},
	},
}

local Align = { provider = "%=" }

ViMode = mode_wrapper({ "", "" }, { ViMode, Snippets })

local DefaultStatusline = {
	ViMode,
	Space,
	FileName,
	Space,
	-- Git,
	-- Space,
	Align,
	FileType,
	Space,
	Position,
	WordCount,
	{
		{
			Space,
			Space,
		},
		condition = function()
			return not conditions.has_diagnostics()
		end,
	},
	Diagnostics,
}
local InactiveStatusline = {
	condition = function()
		return not conditions.is_active()
	end,

	FileName,
	Align,
	FileType,
}

local SpecialStatusline = {
	condition = function()
		return conditions.buffer_matches({
			buftype = { "nofile", "help", "quickfix" },
			filetype = { "^git.*", "fugitive" },
		})
	end,

	HelpFileName,
	QuickfixName,
	Align,
	FileType,
	Space,
	Space,
}

local TerminalStatusline = {

	condition = function()
		return conditions.buffer_matches({ buftype = { "terminal" } })
	end,

	-- Quickly add a condition to the ViMode to only show it when buffer is active!
	{ condition = conditions.is_active, ViMode, Space },
	TerminalName,
	Align,
	FileType,
	Space,
	Space,
}

local StatusLines = {
	hl = function()
		if conditions.is_active() then
			return {
				fg = utils.get_highlight("StatusLine").fg,
				bg = utils.get_highlight("StatusLine").bg,
			}
		else
			return {
				fg = utils.get_highlight("StatusLineNC").fg,
				bg = utils.get_highlight("StatusLineNC").bg,
			}
		end
	end,

	stop_at_first = true,

	SpecialStatusline,
	TerminalStatusline,
	InactiveStatusline,
	DefaultStatusline,
}

return StatusLines
