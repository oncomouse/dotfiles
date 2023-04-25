return {
	{ "folke/lazy.nvim", version = "*" },

	"nvim-lua/plenary.nvim",

	"tpope/vim-repeat",

	-- Editor Enhancements:

	{
		"sickill/vim-pasta",
		event = "VeryLazy",
	}, -- fix block paste for Neovim

	{
		"tpope/vim-sleuth",
		event = { "BufNewFile", "BufReadPost", "BufFilePost", "FileType" },
	}, -- Automatically set indent

	-- Mappings and Commands:

	{
		"vim-scripts/ReplaceWithRegister",
		dependencies = { "vim-repeat" },
		keys = {
			{ "gr", mode = { "x", "n" } },
			{ "grr", mode = "n" },
		},
	}, -- gr{motion} or grr or gr in visual to replace with register

	{
		"oncomouse/sort-motion.nvim",
		dev = false,
		keys = {
			{ "gs", mode = { "x", "n" } },
			{ "gss", mode = { "n" } },
		},
		dependencies = { "vim-repeat" },
	},

	{
		"haya14busa/vim-asterisk",
		keys = {
			{ "*", "<Plug>(asterisk-z*)" },
			{ "#", "<Plug>(asterisk-z#)" },
			{ "g*", "<Plug>(asterisk-gz*)" },
			{ "g#", "<Plug>(asterisk-gz#)" },
		},
		init = function()
			vim.g["asterisk#keeppos"] = 1
		end,
		dependencies = { "vim-repeat" },
	}, -- Fancy * and # bindings

	{
		"oncomouse/lazygit.nvim",
		cmd = "LazyGit",
		keys = { "<Plug>(lazygit.nvim)" },
	}, -- :LazyGit for lazygit integration

	{
		"oncomouse/todo.nvim",
		event = { "BufReadPre", "BufNewFile" },
		opts = {
			maps = {
				jump = {
					next = "gt]",
					prev = "gt[",
				},
			},
		},
	}, -- My todo manager

	{
		"dstein64/vim-startuptime",
		cmd = "StartupTime",
		init = function()
			vim.g.startuptime_tries = 10
		end,
	}, -- :StartupTime for profiling startup

	{
		"oncomouse/nvim-ref",
		dev = false,
		cmd = "NvimRef",
		opts = {
			bibfiles = {
				"~/SeaDrive/My Libraries/My Library/Documents/Academic Stuff/library-test.bib",
			},
		},
	}, -- For BibTeX sources

	{
		"kyazdani42/nvim-web-devicons",
		cond = require("dotfiles.utils.use_termguicolors"),
	}, -- Icons, used in the statusline

	{
		"oncomouse/markdown.nvim",
		dev = false,
		ft = "markdown",
	},
}
