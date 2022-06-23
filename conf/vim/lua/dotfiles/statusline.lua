_stl = {}

-- vim.opt.laststatus = 3 -- Use global statusline

local color = require("dotfiles.statusline.color")
local fm_hrl = require("dotfiles.statusline.from_heirline")

function _stl.WordCount()
	return vim.tbl_contains({
		"markdown",
		"txt",
		"vimwiki",
	}, vim.bo.filetype) and " W:" .. vim.fn.wordcount().words or ""
end

local diagnostic_hl = {}

function _stl.Diagnostics()
	local d = ""
	for sign_key, kind in pairs({
		DiagnosticSignError = vim.diagnostic.severity.ERROR,
		DiagnosticSignWarn = vim.diagnostic.severity.WARN,
		DiagnosticSignInfo = vim.diagnostic.severity.INFO,
		DiagnosticSignHint = vim.diagnostic.severity.HINT,
	}) do
		local c = #vim.diagnostic.get(0, { severity = kind })
		if c ~= 0 then
			-- Get the sign for the given diagnostic group
			local sign = vim.fn.sign_getdefined(sign_key)[1]
			local marker = sign.text
			-- Cache generated diagnostic highlight groups
			if not diagnostic_hl[kind] then
				diagnostic_hl[kind] = vim.api.nvim_get_hl_by_name(sign_key, true)
			end
			-- Get the highlight group
			local h = color.hl({ bg = diagnostic_hl[kind].foreground, fg = 0 })
			d = d .. " " .. h .. marker .. tostring(c) .. "%*"
		end
	end
	return d
end

function _stl.FileName()
	local filename = vim.api.nvim_buf_get_name(0)
	if vim.bo.buftype == "terminal" then
		return " " .. filename:gsub(".*:", "")
	end
	if vim.bo.buftype == "help" then
		return vim.fn.fnamemodify(filename, ":t")
	end
	if vim.bo.buftype == "quickfix" then
		local title = vim.w.quickfix_title or ""
		local name = vim.fn.getwininfo(vim.fn.win_getid())[1].loclist == 1 and "Location List" or "Quickfix List"
		return "[" .. color.hl({ fg = 3 }) .. name .. "%*" .."] " .. title
	end
	if filename == "" then
		return "[No Name]"
	end

	filename = vim.fn.fnamemodify(filename, ":.")
	if fm_hrl.width_percent_below(#filename, 0.25) then
		filename = vim.fn.pathshorten(filename)
	end
	return filename
end

function _stl.FileFlags()
	local flags = ""
	if fm_hrl.buffer_matches({
		buftype =  { "quickfix", "terminal" }
	}) then
		return ""
	end
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
	if fm_hrl.buffer_matches({
		buftype =  { "quickfix", "terminal" }
	}) then
		return ""
	end
	local has_icons, di = pcall(require, "nvim-web-devicons")
	if not has_icons then
		return ""
	end
	local filename = vim.api.nvim_buf_get_name(0)
	local extension = vim.fn.fnamemodify(filename, ":e")
	local icon, icon_color = di.get_icon_color(filename, extension, { default = true })
	return icon and (color.hl({ fg = icon_color }) .. " " .. icon .. "%*") or ""
end

function _stl.FileType()
	if fm_hrl.buffer_matches({
		buftype =  { "quickfix", "terminal" }
	}) then
		return ""
	end
	return "[" .. color.hl({ fg = 3 }) .. vim.bo.filetype .. "%*]"
end

local file_name = "%{%v:lua._stl.FileName()%}%{%v:lua._stl.FileFlags()%}"
local statusline = "%{%v:lua._stl.FileIcon()%} "
	.. file_name
	.. "%=%{%v:lua._stl.FileType()%}%{%v:lua._stl.WordCount()%} %l:%c %p%%%{%v:lua._stl.Diagnostics()%} "
local statusline_nc = "%=" .. file_name .. " "

local function active()
	return vim.g.statusline_winid == vim.fn.win_getid()
end

function _stl.StatusLine()
	return active() and statusline or statusline_nc
end
vim.opt.statusline = "%!v:lua._stl.StatusLine()"
