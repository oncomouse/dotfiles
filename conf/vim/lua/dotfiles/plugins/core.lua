return {
	-- Utilities
	{ "nvim-lua/plenary.nvim", lazy = true }, -- Lua Utilities
	{ "oncomouse/lspize.nvim", lazy = true, dev = false }, -- LSP generator
	{ "kyazdani42/nvim-web-devicons", lazy = true }, -- Icons, used in the statusline

	-- Editor Enhancements:

	"ku1ik/vim-pasta", -- fix block paste for Neovim

	{
		"tpope/vim-sleuth",
		init = function()
			vim.g.sleuth_org_heuristics = false
		end,
		event = { "BufNewFile", "BufReadPost", "BufFilePost", "FileType" },
	}, -- Automatically set indent

	-- Mappings and Commands:

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
	}, -- Fancy * and # bindings

	{
		"oncomouse/lazygit.nvim",
		cmd = "LazyGit",
		keys = {
			{ "<leader>lg", "<Plug>(lazygit.nvim)", desc = "LazyGit Integration" },
		},
	}, -- :LazyGit for lazygit integration

	{
		"dstein64/vim-startuptime",
		cmd = "StartupTime",
		init = function()
			vim.g.startuptime_tries = 10
		end,
	}, -- :StartupTime for profiling startup

	{
		"oncomouse/markdown.nvim",
		dev = false,
	}, -- My custom Markdown plugin
}
