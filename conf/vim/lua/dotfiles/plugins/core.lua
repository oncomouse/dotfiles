return {
	{ "folke/lazy.nvim", version = "*", dev = true },
	"nvim-lua/plenary.nvim",

	-- Editor Enhancements:

	"sickill/vim-pasta", -- fix block paste for Neovim

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
		"ahmedkhalf/project.nvim",
		opts = {
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
		},
		name = "project_nvim",
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

	-- Extra functionality + UI:

	{
		"kyazdani42/nvim-web-devicons",
		cond = require("dotfiles.utils.use_termguicolors"),
	}, -- Icons, used in the statusline

	{
		"oncomouse/nvim-ref",
		dev = false,
		opts = {
			bibfiles = {
				"~/SeaDrive/My Libraries/My Library/Documents/Academic Stuff/library-test.bib",
			}
		}
	},

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

	{
		"NvChad/nvim-colorizer.lua",
		opts = {
			filetypes = {
				"*", -- Load everywhere
				"!packer", -- Except packer buffers
				"!gina*", -- And commit buffers
				html = { names = true, RRGGBBAA = false },
				css = { css = true, RRGGBBAA = false },
				scss = {
					css = true,
					RRGGBBAA = false,
					-- custom_matcher = require("colorizer/sass").variable_matcher,
				},
			},
			user_default_options = {
				names = false, -- Turn off highlighting color words in non-HTML/CSS settings
				RRGGBBAA = true,
				mode = "background", -- Could be background, foreground, or virtualtext
				sass = {
					enable = true,
					parsers = {
						css = true,
					},
				},
			},
		},
	}, -- Highlight colors in files

	{
		"lukas-reineke/indent-blankline.nvim",
		event = "VeryLazy",
		opts = {
			show_end_of_line = true,
			space_char_blankline = " ",
			show_current_context = true,
		},
	}, -- Mark and highlight indentations
}
