local addtional_path_prefix = os.getenv("HOME") .. "/dotfiles/conf/wezterm/"
local addtional_path = ";" .. addtional_path_prefix .. "?/init.lua;" .. addtional_path_prefix .. "?.lua"
package.path = package.path .. addtional_path

local wezterm = require("wezterm")
local table_merge = require("table_merge").table_merge

local HOME = os.getenv("HOME")
local XDG_CACHE_HOME = os.getenv("XDG_CACHE_HOME") or HOME .. "/.cache"
local XDG_CONFIG_HOME = os.getenv("XDG_CONFIG_HOME") or HOME .. "/.config"
local is_laptop = os.getenv("DOTFILES_TARGET") == "laptop"

local function nerdify_font(font, params)
	font = type(font) == "table" and font or {
		family = font,
	}
	return wezterm.font_with_fallback({
		font,
		{
			family = "Fira Code Nerd Font",
		},
	}, params)
end

local function get_fonts()
	if is_laptop then
		return {
			font = nerdify_font("JetBrains Mono"),
			font_size = 13.0,
		}
	end
	return {
		font = nerdify_font("Fira Code"),
		font_rules = {
			{
				italic = true,
				font = nerdify_font("Hasklig", { italic = true }),
			},
		},
		font_size = 18.0,
		harfbuzz_features = { "zero", "ss02" },
	}
end

return table_merge({
	-- Color scheme:
	color_scheme_dirs = {
		XDG_CACHE_HOME .. "/wal",
	},
	color_scheme = "wal",
	hide_tab_bar_if_only_one_tab = true,
	keys = {
		{
			key = "Enter",
			mods = "ALT",
			action = "DisableDefaultAssignment"
		},
		{
			key = "1",
			mods = "ALT",
			action = "DisableDefaultAssignment"
		},
		{
			key = "2",
			mods = "ALT",
			action = "DisableDefaultAssignment"
		},
		{
			key = "3",
			mods = "ALT",
			action = "DisableDefaultAssignment"
		},
		{
			key = "4",
			mods = "ALT",
			action = "DisableDefaultAssignment"
		},
		{
			key = "5",
			mods = "ALT",
			action = "DisableDefaultAssignment"
		},
		{
			key = "6",
			mods = "ALT",
			action = "DisableDefaultAssignment"
		},
		{
			key = "7",
			mods = "ALT",
			action = "DisableDefaultAssignment"
		},
		{
			key = "8",
			mods = "ALT",
			action = "DisableDefaultAssignment"
		},
		{
			key = "9",
			mods = "ALT",
			action = "DisableDefaultAssignment"
		},
	}
}, get_fonts())
