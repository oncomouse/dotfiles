return {
	{
		"folke/lazy.nvim",
		version = "*",
		keys = {
			{ "<leader>la", "<cmd>Lazy<cr>", desc = "Lazy.nvim" },
			{ "<leader>lu", "<cmd>Lazy update<cr>", desc = "Update Lazy.nvim plugins" },
		},
	},

	{ "nvim-lua/plenary.nvim", lazy = true },

	"tpope/vim-repeat",

	-- Editor Enhancements:

	{
		"sickill/vim-pasta",
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
			{ "gr", mode = { "x", "n" }, desc = "Replace region with register" },
			{ "grr", mode = "n", desc = "Replace line with register" },
		},
	}, -- gr{motion} or grr or gr in visual to replace with register

	{
		"oncomouse/sort-motion.nvim",
		dev = false,
		keys = {
			{ "gs", mode = { "x", "n" }, desc = "Sort region" },
			{ "gss", mode = { "n" }, desc = "Sort line" },
		},
		dependencies = { "vim-repeat" },
	},

	{
		"haya14busa/vim-asterisk",
		keys = {
			{
				"*",
				"<Plug>(asterisk-z*)",
				desc = "Search forward for the [count]'th occurrence of the word nearest to the cursor",
			},
			{
				"#",
				"<Plug>(asterisk-z#)",
				desc = "Search backward for the [count]'th occurrence of the word nearest to the cursor",
			},
			{
				"g*",
				"<Plug>(asterisk-gz*)",
				desc = "Search forward for the [count]'th occurrence of the word (or part of a word) nearest to the cursor",
			},
			{
				"g#",
				"<Plug>(asterisk-gz#)",
				desc = "Search backward for the [count]'th occurrence of the word (or part of a word) nearest to the cursor",
			},
		},
		init = function()
			vim.g["asterisk#keeppos"] = 1
		end,
		dependencies = { "vim-repeat" },
	}, -- Fancy * and # bindings

	{
		"oncomouse/lazygit.nvim",
		cmd = "LazyGit",
		keys = {
			{ "<leader>lg", "<Plug>(lazygit.nvim)", desc = "LazyGit Integration" },
		},
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
		dev = true,
		cmd = "NvimRef",
		init = function()
			vim.g.nvim_ref_options = {
				bibfiles = {
					"~/SeaDrive/My Libraries/My Library/Documents/Academic Stuff/library-test.bib",
				},
			}
		end,
		dependencies = {
			"plenary.nvim",
		},
	}, -- For BibTeX sources

	{
		"kyazdani42/nvim-web-devicons",
		lazy = true,
		cond = require("dotfiles.utils.use_termguicolors"),
	}, -- Icons, used in the statusline

	{
		"oncomouse/markdown.nvim",
		dev = false,
		ft = "markdown",
	},
}
