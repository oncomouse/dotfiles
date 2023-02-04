-- Add Dotfiles To RTP:
vim.opt.runtimepath:append("~/dotfiles/conf/vim")
vim.opt.runtimepath:append("~/dotfiles/conf/vim/after")
-- Autogroups
vim.api.nvim_create_augroup("dotfiles-settings", { clear = true })
-- Set Leader:
vim.g.mapleader = " "
vim.g.maplocalleader = ","

local xdg = require("dotfiles.utils.xdg")
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
require("lazy").setup(require("dotfiles.plugins"), {
	performance = {
		rtp = {
			reset = true,
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
local things_to_load = {
	"options",
	"autocmds",
	"commands",
	"maps",
}
local function load_things()
	for _, thing in pairs(things_to_load) do
		require("dotfiles." .. thing)
	end
end

if vim.fn.argc(-1) == 0 then
	-- autocmds and keymaps can wait to load
	vim.api.nvim_create_autocmd("User", {
		group = vim.api.nvim_create_augroup("LazyVim", { clear = true }),
		pattern = "VeryLazy",
		callback = load_things,
	})
else
	-- load them now so they affect the opened buffers
	load_things()
end
-- # vim:foldmethod=marker:foldlevel=0
