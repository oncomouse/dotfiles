return {
	{
		"folke/lazy.nvim",
		version = "*",
		keys = {
			{ "<leader>la", "<cmd>Lazy<cr>",        desc = "Lazy.nvim" },
			{ "<leader>lu", "<cmd>Lazy update<cr>", desc = "Update Lazy.nvim plugins" },
		},
	},

	-- Utilities
	{ "nvim-lua/plenary.nvim", lazy = true },
	{ "oncomouse/lspize.nvim", lazy = true, dev = false },

	"tpope/vim-repeat",

	-- Editor Enhancements:

	"sickill/vim-pasta", -- fix block paste for Neovim

	{
		"tpope/vim-sleuth",
		init = function()
			vim.g.sleuth_org_heuristics = false
		end,
		event = { "BufNewFile", "BufReadPost", "BufFilePost", "FileType" },
	}, -- Automatically set indent

	-- Mappings and Commands:

	{
		"vim-scripts/ReplaceWithRegister",
		dependencies = { "vim-repeat" },
		keys = {
			{ "gr",  mode = { "x", "n" }, desc = "Replace region with register" },
			{ "grr", mode = "n",          desc = "Replace line with register" },
		},
	}, -- gr{motion} or grr or gr in visual to replace with register

	{
		"oncomouse/sort-motion.nvim",
		dev = false,
		keys = {
			{ "gs",  mode = { "x", "n" }, desc = "Sort region" },
			{ "gss", mode = { "n" },      desc = "Sort line" },
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
				desc =
				"Search forward for the [count]'th occurrence of the word (or part of a word) nearest to the cursor",
			},
			{
				"g#",
				"<Plug>(asterisk-gz#)",
				desc =
				"Search backward for the [count]'th occurrence of the word (or part of a word) nearest to the cursor",
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
		dev = false,
		ft = {
			"latex",
			"markdown",
			"org",
		},
		cmd = "NvimRef",
		opts = {
			bibfiles = {
				"~/SeaDrive/My Libraries/My Library/Documents/Academic Stuff/library.bib",
			},
		},
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
	},

	{
		"dimfeld/section-wordcount.nvim",
		lazy = true,
		opts = {
			highlight = "String",
			virt_text_pos = "eol",
		},
		init = function()
			local ag = vim.api.nvim_create_augroup("dotfiles-wordcount-autocmd", { clear = true })

			vim.api.nvim_create_autocmd("FileType", {
				pattern = "markdown",
				group = ag,
				callback = function() require("section-wordcount").wordcounter() end,
			})
		end
	},

	{
		"rcarriga/nvim-notify",
		opts = {
			timeout = 3000,
		},
		config = function(_, opts)
			require("notify").setup(opts)
			vim.notify = require("notify")
		end
	},
}
