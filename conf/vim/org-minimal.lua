-- Set Leader:
vim.g.mapleader = " "
local nvim_root = "/tmp/nvim_orgmode"
local lazy_root = nvim_root .. "/lazy"
local lazypath = lazy_root .. "/lazy.nvim"
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
	root = lazy_root,
	lockfile = nvim_root .. "/lazy.json",
	dev = true,
	opts = {
		org_default_notes_file = "~/org/inbox.org",
	},
	spec = {
		{
			"nvim-orgmode/orgmode",
			config = function(_, opts)
				require("orgmode").setup_ts_grammar()
				require("nvim-treesitter.configs").setup({
					highlight = {
						enable = true,
						additional_vim_regex_highlighting = { "org" },
					},
					ensure_installed = { "org" },
				})
				require("orgmode").setup(opts)
			end,
			dependencies = {
				{
					"nvim-treesitter/nvim-treesitter",
				},
			},
		},
	},
})
-- # vim:ts=2:sw=2:sts=2:expandtab
