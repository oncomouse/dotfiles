_stl = {}

-- Color
local hlgs = {}
local function hl_name(h)
	return "Sl"
		.. (h.fg and string.gsub(h.fg, "#", "") or "_")
		.. (h.bg and string.gsub(h.bg, "#", "") or "_")
		.. (h.sp and string.gsub(h.sp, "#", "") or "_")
end

local function rgb_or_ansi(color)
	return type(color) == "string" and color or vim.api.nvim_get_hl_by_name("AnsiColor" .. color, true).foreground
end

local function hl_group(h, n)
	local no = vim.api.nvim_get_hl_by_name("Normal", true)
	local hl = {}
	hl.foreground = h.fg and rgb_or_ansi(h.fg) or no.foreground
	hl.background = h.bg and rgb_or_ansi(h.bg) or no.background
	hl.special = h.sp and rgb_or_ansi(h.sp) or no.background
	for _, k in pairs(vim.tbl_filter(function(x)
		return not vim.tbl_contains({ "fg", "bg", "sp" }, x)
	end, vim.tbl_keys(h))) do
		hl[k] = h[k]
	end
	vim.api.nvim_set_hl(0, n, hl)
end
local function hl(h)
	local n = hl_name(h)
	if not hlgs[n] then
		hl_group(h, n)
	end
	return "%#" .. n .. "#"
end
-- End Color

function _stl.WordCount()
	return vim.tbl_contains({
		"markdown",
		"txt",
		"vimwiki",
	}, vim.opt.filetype:get()) and " W:" .. vim.fn.wordcount().words or ""
end
function _stl.Diagnostics()
	local d = ""
	for sign_key, kind in pairs({
		DiagnosticSignError = vim.diagnostic.severity.ERROR,
		DiagnosticSignWarn = vim.diagnostic.severity.WARN,
		DiagnosticSignInfo = vim.diagnostic.severity.INFO,
		DiagnosticSignHint = vim.diagnostic.severity.HINT,
	}) do
		local c = #vim.diagnostic.get(0, { severity = kind })
		local sign = vim.fn.sign_getdefined(sign_key)[1]
		local marker = sign.text
		if c ~= 0 then
			d = d .. " " .. marker .. tostring(c)
		end
	end
	return d
end

-- From heirline.nvim
local function width_percent_below(n, thresh, is_winbar)
	local winwidth
	if vim.o.laststatus == 3 and not is_winbar then
		winwidth = vim.o.columns
	else
		winwidth = vim.api.nvim_win_get_width(0)
	end

	return n / winwidth <= thresh
end
-- End From heirline.nvim

function _stl.FileName()
	local filename = vim.api.nvim_buf_get_name(0)
	-- first, trim the pattern relative to the current directory. For other
	-- options, see :h filename-modifers
	filename = vim.fn.fnamemodify(filename, ":.")
	if filename == "" then
		return "[No Name]"
	end
	if vim.bo.buftype == "help" then
		return vim.fn.fnamemodify(filename, ":t")
	end
	-- now, if the filename would occupy more than 1/4th of the available
	-- space, we trim the file path to its initials
	if width_percent_below(#filename, 0.25) then
		filename = vim.fn.pathshorten(filename)
	end
	return filename
end

function _stl.FileFlags()
	local flags = ""
	if vim.bo.modified then
		flags = flags .. "[+]"
	end
	if vim.bo.buftype == "help" then
		flags = flags .. "[?]"
	end
	if not vim.bo.modifiable or vim.bo.readonly then
		flags = flags .. "[]"
	end
	return flags
end

function _stl.FileIcon()
	local filename = vim.api.nvim_buf_get_name(0)
	local extension = vim.fn.fnamemodify(filename, ":e")
	local icon, icon_color = require("nvim-web-devicons").get_icon_color(filename, extension, { default = true })
	return icon and (hl({ fg = icon_color, bg = 8 }) .. icon .. "%*") or ""
end

function _stl.FileType()
	return "[" .. hl({ fg = 3, bg = 8 }) .. vim.bo.filetype .. "%*]"
end

local file_name = " %{%v:lua._stl.FileName()%}%{%v:lua._stl.FileFlags()%}"
local statusline = " %{%v:lua._stl.FileIcon()%}"
	.. file_name
	.. "%=%{%v:lua._stl.FileType()%}%{%v:lua._stl.WordCount()%} %l:%c %p%%%{%v:lua._stl.Diagnostics()%} "
local statusline_nc = "%=" .. file_name

local function active()
	return vim.g.statusline_winid == vim.fn.win_getid()
end

function _stl.StatusLine()
	return active() and statusline or statusline_nc
end
vim.opt.statusline = "%!v:lua._stl.StatusLine()"
