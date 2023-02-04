-- Autogroups
vim.api.nvim_create_augroup("dotfiles-settings", { clear = true })
-- Set Leader:
vim.g.mapleader = " "
vim.g.maplocalleader = ","

local function xdg_default(v, d)
	local o = os.getenv(v)
	return o and o or os.getenv("HOME") .. d
end
local xdg = function(var_name)
	if var_name == "XDG_CONFIG_HOME" then
		return xdg_default("XDG_CONFIG_HOME", "/.config")
	elseif var_name == "XDG_CACHE_HOME" then
		return xdg_default("XDG_CACHE_HOME", "/.cache")
	elseif var_name == "XDG_DATA_HOME" then
		return xdg_default("XDG_DATA_HOME", "/.local/share")
	end
	return nil
end

local lazypath = xdg("XDG_DATA_HOME") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)
require("lazy").setup({
	spec = {
		{ import = "dotfiles.plugins" },
	},
	performance = {
		dev = {
			path = "~/Projects",
		},
		rtp = {
			paths = {
				"~/dotfiles/conf/vim",
				"~/dotfiles/conf/vim/after",
			},
			disabled_plugins = {
				"gzip",
				"matchit",
				"matchparen",
				"netrwPlugin",
				"tarPlugin",
				"tohtml",
				"tutor",
				"zipPlugin",
			},
		},
	},
})
for _, thing in pairs({
	"options",
	"autocmds",
	"commands",
	"maps",
}) do
	require("dotfiles." .. thing)
end
-- # vim:foldmethod=marker:foldlevel=0
