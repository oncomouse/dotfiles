-- Color
-- Cache generated highlight groups
local hlgs = {}

-- Make the highlight group's name
local function hl_name(h)
	return "Sl"
		.. (h.fg and string.gsub(h.fg, "#", "") or "_")
		.. (h.bg and string.gsub(h.bg, "#", "") or "_")
		.. (h.sp and string.gsub(h.sp, "#", "") or "_")
end

-- Process the color the way we want
local function rgb_or_ansi(color)
	-- Is this a hex string?
	if type(color) == "string" then
		return color
	end
	-- Is this an integer returned from nvim_get_hl_by_name?
	if color > 15 then
		return color
	end
	-- Otherwise, try for an AnsiColorX highlight grouping
	local ok, hi = pcall(vim.api.nvim_get_hl_by_name, "AnsiColor" .. color, true)
	return ok and hi.foreground or "None"
end

local function default_hl()
	if not hlgs["default"] then
		hlgs["default"] = vim.api.nvim_get_hl_by_name("StatusLine", true)
	end
	return hlgs["default"]
end

-- Generate a highlight group with h characteristics and named "n"
local function hl_group(h, n)
	local hl = {}
	local no = default_hl()

	hl.foreground = h.fg and rgb_or_ansi(h.fg) or no.foreground
	if type(h.fg) == "number" and h.fg <= 15 then
		hl.ctermfg = h.fg
	end

	hl.background = h.bg and rgb_or_ansi(h.bg) or no.background
	if type(h.bg) == "number" and h.bg <= 15 then
		hl.ctermbg = h.bg
	end

	hl.special = h.sp and rgb_or_ansi(h.sp) or no.background

	-- Include any other keys from the original object
	for _, k in
		pairs(vim.tbl_filter(function(x)
			return not vim.tbl_contains({ "fg", "bg", "sp" }, x)
		end, vim.tbl_keys(h)))
	do
		hl[k] = h[k]
	end

	vim.api.nvim_set_hl(0, n, hl)
end

-- Make the highlight grouping for use in statusline functions
local function hl(h)
	local n = hl_name(h)
	if not hlgs[n] then
		hl_group(h, n)
	end
	return "%#" .. n .. "#"
end

local function regenerate_colors()
	hlgs = {}
end

return { hl = hl, regenerate_colors = regenerate_colors }
