return {
	{ "nvim-lua/plenary.nvim" },

	-- Editor Enhancements:

	"sickill/vim-pasta", -- fix block paste for Neovim

	"zdcthomas/yop.nvim", -- Custom operators (gs for sort)
	-- Configured in ~/dotfiles/conf/vim/after/plugin/yop-nvim.lua

	"tpope/vim-sleuth", -- Automatically set indent

	{
		"vim-scripts/ReplaceWithRegister",
		dependencies = { "tpope/vim-repeat" },
	}, -- gr{motion} or grr or gr in visual to replace with register

	{
		"tpope/vim-unimpaired",
		dependencies = { "tpope/vim-repeat" },
	},

	{
		"echasnovski/mini.nvim",
		dependencies = {
			{ "preservim/vim-textobj-sentence" }, -- Sentence object
		},
		-- Configured in ~/dotfiles/conf/vim/after/plugin/mini-nvim.lua
	}, -- Lots of plugins. We use mini.ai for textobjects; mini.comment for commenting; mini.indentscope for indent-based textobjects (ii, ai); mini.surround for surround (ys to add, cs to change, ds to delete)

	{
		"ahmedkhalf/project.nvim",
		config = function()
			require("project_nvim").setup({
				patterns = {
					".git/",
					"Gemfile",
					"Makefile",
					"Rakefile",
					"package.json",
					"pyproject.toml",
					"setup.py",
					".project-root",
				},
			})
		end,
	}, -- Set project root

	{
		"haya14busa/vim-asterisk",
		config = function()
			vim.keymap.set("", "*", "<Plug>(asterisk-z*)")
			vim.keymap.set("", "#", "<Plug>(asterisk-z#)")
			vim.keymap.set("", "g*", "<Plug>(asterisk-gz*)")
			vim.keymap.set("", "g#", "<Plug>(asterisk-gz#)")
			vim.g["asterisk#keeppos"] = 1
		end,
		dependencies = { "tpope/vim-repeat" },
	}, -- Fancy * and # bindings

	{
		"cohama/lexima.vim", -- Autopairs
		event = "InsertEnter",
		init = function()
			vim.g.lexima_map_escape = ""
		end,
		config = function()
			require("dotfiles.plugins.lexima").setup()
			-- Autoclose mapping:
			vim.keymap.set("i", "<C-l>", "<Plug>(dotfiles-lexima-leave-til-eol)", { silent = true })
		end,
		-- Configured in ~/dotfiles/conf/vim/after/plugin/lexima.lua
	}, -- Endwise and autopairs

	-- Extra functionality + UI:

	{
		"kyazdani42/nvim-web-devicons",
		cond = require("dotfiles.utils.use_termguicolors"),
	}, -- Icons, used in the statusline

	{
		"ibhagwan/fzf-lua",
		keys = { { mode = "n", "<C-p>", "<Plug>(dotfiles-fzf-files)" }, { mode = "n", "<Leader>a", "<Plug>(dotfiles-fzf-buffers)" } },
		cmd = { "GitStatus", "Files", "Buffers" },
		init = function() -- Shim vim.ui.select until we can load the plugin
			vim.ui.select = function(...)
				require("fzf-lua.providers.ui_select").ui_select(...)
			end
		end,
		config = function()
			require("dotfiles.plugins.fzf-lua").setup()
		end,
		-- Configured in ~/dotfiles/conf/vim/lua/dotfiles/plugins/fzf-lua.lua
	}, -- FZF Client

	{
		"monaqa/dial.nvim",
		keys = {
			{ "<C-a>", "<Plug>(dial-increment)", mode = "n" },
			{ "<C-x>", "<Plug>(dial-decrement)", mode = "n" },
			{ "<C-a>", "<Plug>(dial-increment)", mode = "v" },
			{ "<C-x>", "<Plug>(dial-decrement)", mode = "v" },
			{ "g<C-a>", "g<Plug>(dial-increment)", mode = "v" },
			{ "g<C-x>", "g<Plug>(dial-decrement)", mode = "v" },
		},
		config = function()
			local augend = require("dial.augend")
			require("dial.config").augends:register_group({
				-- default augends used when no group name is specified
				default = {
					augend.integer.alias.decimal, -- nonnegative decimal number (0, 1, 2, 3, ...)
					augend.integer.alias.hex, -- nonnegative hex number  (0x01, 0x1a1f, etc.)
					augend.constant.alias.bool, -- boolean value (true <-> false)
					augend.date.alias["%Y/%m/%d"], -- date (2022/02/18, etc.)
					-- augend.date.alias["%m/%d/%Y"], -- date (02/19/2022)
				},
			})
		end,
	},

	(vim.g.nvim_ref_devel and {
		"~/Projects/nvim-ref",
		config = function()
			require("nvim-ref").setup({
				bibfiles = {
					"~/SeaDrive/My Libraries/My Library/Documents/Academic Stuff/library-test.bib",
				},
			})
		end,
		rocks = {
			{ "lpeg-bibtex", server = "https://luarocks.org/dev" },
		},
	} or {
		"oncomouse/nvim-ref",
		config = function()
			require("nvim-ref").setup({
				bibfiles = { vim.g.bibfiles },
			})
		end,
		rocks = {
			{ "lpeg-bibtex", server = "https://luarocks.org/dev" },
		},
	}),

	{
		"L3MON4D3/LuaSnip",
		dependencies = {
			{
				"rafamadriz/friendly-snippets",
			}, -- Base Snippets
		},
		-- Configured in ~/dotfiles/conf/vim/lua/dotfiles/plugins/luasnip.lua
	}, -- Snippets

	"jose-elias-alvarez/null-ls.nvim", -- Diagnostics and Formatting
	-- Configured in ~/dotfiles/conf/vim/after/plugin/null-ls.lua

	"neovim/nvim-lspconfig", -- LSP
	-- Configured in ~/dotfiles/conf/vim/plugin/nvim-lspconfig.lua

	{
		"nvim-treesitter/nvim-treesitter",
		build = function()
			vim.cmd([[TSUpdate]])
		end,
		dependencies = {
			{
				"JoosepAlviste/nvim-ts-context-commentstring", -- Contextual commentstring
				config = function()
					require("mini.comment").setup({
						hooks = {
							pre = function()
								require("ts_context_commentstring.internal").update_commentstring()
							end,
						},
					})
				end,
			},
			{ "nvim-treesitter/nvim-treesitter-textobjects" }, -- Configuration for treesitter objects
			{ "windwp/nvim-ts-autotag" },
			{
				"andymass/vim-matchup",
				init = function()
					vim.g.matchup_matchparen_offscreen = {
						method = "popup",
					}
				end,
			},
		},
		-- Configured in ~/dotfiles/conf/vim/plugin/nvim-treesitter.lua
	}, -- Treesitter-based Syntax

	{
		"oncomouse/vim-markdown",
		ft = "markdown",
		init = function()
			vim.g.vim_markdown_frontmatter = 1 -- Format YAML
			vim.g.vim_markdown_strikethrough = 1 -- Don't format strikethrough
			vim.g.vim_markdown_conceal = 0 -- Don't conceal
			vim.g.vim_markdown_conceal_code_blocks = 0 -- Don't conceal code blocks
			vim.g.vim_markdown_math = 1 -- Do process MathJaX and LaTeX math
			vim.g.vim_markdown_auto_insert_bullets = 0 -- autoList handles bullet insertion now
			vim.g.vim_markdown_new_list_item_indent = 0 -- autoList handles spacing for lists
		end,
	}, -- Markdown Syntax

	-- Appearance:

	{ "oncomouse/catppuccin.nvim", name = "catppuccin" }, -- Theme (catppuccin with cterm support)

	"NvChad/nvim-colorizer.lua", -- Highlight colors in files
	-- Configured in ~/dotfiles/conf/vim/plugin/nvim-colorizer.lua

	{
		"rebelot/heirline.nvim",
		dependencies = {
			"oncomouse/czs.nvim",
		},
	}, -- Statusline
	-- Configured in ~/dotfiles/conf/vim/after/plugin/heirline.lua

	{
		"lukas-reineke/indent-blankline.nvim",
		event = "VeryLazy",
	}, -- Mark and highlight indentations
	-- Configured in ~/dotfiles/conf/vim/after/plugin/indent-blankline.lua
}
