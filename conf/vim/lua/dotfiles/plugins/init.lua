return require("packer").startup({
	function(use)
		use({
			{ "wbthomason/packer.nvim", opt = true },
			"sickill/vim-pasta", -- fix block paste for Neovim
			{
				"christoomey/vim-sort-motion",
				keys = {
					{ "x", "gs" },
					{ "n", "gs" },
					{ "n", "gss" },
				},
			}, -- gs to sort
			{ "tpope/vim-commentary", requires = { "tpope/vim-repeat" } }, -- gc<motion> to (un)comment
			{
				"oncomouse/vim-surround",
				requires = { "tpope/vim-repeat" },
				keys = {
					{ "x", "S" },
					{ "n", "ys" },
					{ "n", "yss" },
					{ "n", "ds" },
					{ "n", "cs" },
				},
			}, -- ys to add, cs to change, ds to delete. f, F for function, t, T for tag
			{ "wellle/targets.vim", requires = { "tpope/vim-repeat" } }, -- add next block n]) targets, plus words in commas (a,), asterisks (a*), etc
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
						},
					})
				end,
			}, -- Set project root
			-- Editor Enhancements:
			{
				"oncomouse/vim-lion",
				keys = {
					{ "v", "gl" },
					{ "n", "gl" },
					{ "v", "gL" },
					{ "n", "gL" },
				},
			}, -- gl and gL to align
			{
				"haya14busa/vim-asterisk",
				config = function()
					vim.keymap.set("", "*", "<Plug>(asterisk-*)")
					vim.keymap.set("", "#", "<Plug>(asterisk-#)")
					vim.keymap.set("", "g*", "<Plug>(asterisk-g*)")
					vim.keymap.set("", "g#", "<Plug>(asterisk-g#)")
					vim.keymap.set("", "z*", "<Plug>(asterisk-z*)")
					vim.keymap.set("", "gz*", "<Plug>(asterisk-gz*)")
					vim.keymap.set("", "z#", "<Plug>(asterisk-z#)")
					vim.keymap.set("", "gz#", "<Plug>(asterisk-gz#)")
					vim.g["asterisk#keeppos"] = 1
				end,
				requires = { "tpope/vim-repeat" },
			}, -- Fancy * and # bindings
			{
				"vim-scripts/ReplaceWithRegister",

				keys = {
					{ "x", "gr" },
					{ "n", "gr" },
					{ "n", "grr" },
				},
				requires = { "tpope/vim-repeat" },
			}, -- gr{motion} or grr or gr in visual to replace with register
			{
				"cohama/lexima.vim",
			}, -- Autopairs + Endwise
			-- Extra functionality + UI:
			{
				"ibhagwan/fzf-lua",
				cmd = { "Files", "Buffers", "GitStatus" },
				config = function()
					local wh = vim.api.nvim_win_get_height(0)
					local ph = vim.o.previewheight
					require("fzf-lua").setup({
						winopts = {
							height = ph / wh,
							width = 1.0,
							row = 0.99,
							col = 0,
							border = "none",
							preview = {
								flip_columns = 110,
							},
						},
						files = {
							actions = {
								["default"] = require("fzf-lua.actions").file_edit,
							},
						},
						git = {
							status = {
								actions = {
									["default"] = function()
										vim.cmd("Gina commit")
									end,
								},
							},
						},
						fzf_colors = {
							["fg"] = { "fg", "Normal" },
							["bg"] = { "bg", "Normal" },
							["hl"] = { "fg", "Comment" },
							["fg+"] = { "fg", "Normal" },
							["bg+"] = { "bg", "CursorLine" },
							["hl+"] = { "fg", "Statement" },
							["info"] = { "fg", "PreProc" },
							["prompt"] = { "fg", "Label" },
							["pointer"] = { "fg", "Identifier" },
							["marker"] = { "fg", "Identifier" },
							["spinner"] = { "fg", "Identiier" },
							["header"] = { "fg", "Comment" },
							["gutter"] = { "bg", "Normal" },
						},
					})
					vim.api.nvim_create_user_command("Files", function(args)
						require("fzf-lua").files({
							fzf_opts = { ["--layout"] = "reverse-list", ["--info"] = "inline" },
							cwd = args.args == "" and "." or args.args,
						})
					end, {
						complete = "dir",
						force = true,
						nargs = "?",
					})
					vim.api.nvim_create_user_command("Buffers", function()
						require("fzf-lua").buffers()
					end, {
						force = true,
					})
					vim.api.nvim_create_user_command("GitStatus", function()
						require("fzf-lua").git_status()
					end, {
						force = true,
					})
				end,
				requires = { "kyazdani42/nvim-web-devicons" },
			}, -- FZF Client
			{
				"lambdalisue/gina.vim",
				cmd = "Gina",
				config = function()
					for _, command in pairs({
						"branch",
						"changes",
						"commit",
						"diff",
						"log",
						"status",
					}) do
						vim.fn["gina#custom#command#option"](
							command,
							"--opener",
							vim.opt.previewheight:get() .. "split"
						)
						vim.fn["gina#custom#command#option"](command, "--group", "short")
					end
					-- Implement vim-fugitive commands in Gina:
					vim.fn["gina#custom#mapping#nmap"]("status", "cc", ":<C-u>Gina commit<CR>", {
						noremap = 1,
						silent = 1,
					})
				end,
			}, -- Git support
			{
				"L3MON4D3/LuaSnip",
				requires = {
					{ "rafamadriz/friendly-snippets", after = { "LuaSnip" } }, -- Base Snippets
					{ "edheltzel/vscode-jekyll-snippets", ft = { "markdown", "html" } }, -- Jekyll Snippets
				},
			},
			-- {
			-- 	"hrsh7th/vim-vsnip",
			-- 	event = "VimEnter",
			-- 	config = function()
			-- 		vim.g.vsnip_snippet_dir = os.getenv("HOME") .. "/dotfiles/conf/vim/snippets"
			-- 		vim.keymap.set(
			-- 			{ "s", "i" },
			-- 			"<Tab>",
			-- 			"vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : '<Tab>'",
			-- 			{ expr = true, remap = true }
			-- 		)
			-- 		vim.keymap.set(
			-- 			{ "s", "i" },
			-- 			"<S-Tab>",
			-- 			"vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'",
			-- 			{ expr = true, remap = true }
			-- 		)
			-- 		vim.api.nvim_create_autocmd("CompleteDone", {
			-- 			group = "dotfiles-settings",
			-- 			command = "if vsnip#available(1) | call vsnip#expand() | endif",
			-- 		})
			-- 	end,
			-- 	requires = {
			-- 		{ "rafamadriz/friendly-snippets", after = { "vim-vsnip" } }, -- Base Snippets
			-- 		{ "edheltzel/vscode-jekyll-snippets", ft = { "markdown", "html" } }, -- Jekyll Snippets
			-- 	},
			-- }, -- Snippets
			{
				"neovim/nvim-lspconfig",
				requires = {
					{ "williamboman/nvim-lsp-installer", module = "nvim-lsp-installer" },
					{
						"jose-elias-alvarez/null-ls.nvim",
						module = "null-ls",
						requires = { { "nvim-lua/plenary.nvim", module = "plenary" } },
					},
				},
				ft = {
					"css",
					"fish",
					"html",
					"javascript",
					"javascriptreact",
					"json",
					"jsonc",
					"lua",
					"markdown",
					"python",
					"ruby",
					"scss",
					"sh",
					"typescript",
					"typescriptreact",
					"vim",
					"yaml",
				},
				config = function()
					require("dotfiles.nvim-lsp")
				end,
			}, -- LSP
			{
				"nvim-treesitter/nvim-treesitter",
				run = function()
					vim.cmd([[TSUpdate]])
				end,
				config = function()
					vim.opt_local.foldexpr = "nvim_treesitter#foldexpr()"
					vim.opt_local.foldmethod = "expr"
					require("nvim-treesitter.configs").setup({
						ensure_installed = {
							"bash",
							"bibtex",
							"c",
							"cmake",
							"comment",
							"cpp",
							"css",
							"dockerfile",
							"fennel",
							"fish",
							"go",
							"graphql",
							"html",
							"http",
							"java",
							"javascript",
							"jsdoc",
							"json",
							"jsonc",
							"latex",
							"lua",
							"make",
							"ninja",
							"nix",
							"norg",
							"org",
							"perl",
							"php",
							"python",
							"r",
							"rasi",
							"regex",
							"ruby",
							"rust",
							"scss",
							"tsx",
							"typescript",
							"vim",
							"vue",
							"yaml",
							"zig",
						},
						highlight = { enable = true },
					})
				end,
				requires = {
					{
						"JoosepAlviste/nvim-ts-context-commentstring", -- Contextual commentstring
						ft = {
							"javascript",
							"javascriptreact",
							"typescript",
							"typescriptreact",
							"css",
							"scss",
							"html",
							"lua",
							"vim",
						},
						config = function()
							require("nvim-treesitter.configs").setup({
								context_commentstring = {
									enable = true,
								},
							})
						end,
					},
				},
			}, -- Treesitter-based Syntax
			-- Non-Treesitter Syntax:
			{
				"plasticboy/vim-markdown",
				ft = "markdown",
				setup = function()
					vim.g.vim_markdown_frontmatter = 1 -- Format YAML
					vim.g.vim_markdown_strikethrough = 1 -- Don't format strikethrough
					vim.g.vim_markdown_conceal = 0 -- Don't conceal
					vim.g.vim_markdown_conceal_code_blocks = 0 -- Don't conceal code blocks
					vim.g.vim_markdown_math = 1 -- Do process MathJaX and LaTeX math
				end,
				requires = {
					{ -- Required for TableFormat in vim-markdown but also useful elsewhere
						"godlygeek/tabular",
						cmd = { "Tabularize" },
					},
				},
			}, -- Markdown Syntax
			-- Appearance:
			{
				"lukas-reineke/indent-blankline.nvim",
				config = function()
					require("indent_blankline").setup({
						buftype_exclude = { "terminal" },
						filetype_exclude = {
							"diff",
							"fzf",
							"gina-status",
							"help",
							"lsp-installer",
							"markdown",
							"packer",
							"qf",
						},
						show_current_context = true,
						context_patterns = { "class", "function", "method", "^if", "table", "^for", "^while" },
						show_current_context_start = true,
					})
				end,
			},
			{
				"oncomouse/lushwal.nvim",
				requires = { { "rktjmp/lush.nvim", opt = true }, { "rktjmp/shipwright.nvim", opt = true } },
				config = function()
					vim.g.lushwal_configuration = {
						addons = {
							hop_nvim = true,
							indent_blankline_nvim = true,
							gina = true,
							markdown = true,
						},
					}
				end,
			}, -- Colorscheme
			{
				"oncomouse/nvim-colorizer.lua",
				config = function()
					if vim.opt.termguicolors:get() then
						require("colorizer").setup({
							"*",
							"!packer",
							html = { names = true, RRGGBBAA = false },
							css = { css = true, RRGGBBAA = false },
							scss = {
								css = true,
								RRGGBBAA = false,
								custom_matcher = require("colorizer/sass").variable_matcher,
							},
						}, {
							names = false,
							RRGGBBAA = true,
							mode = "background", -- Could be background, foreground, or virtualtext
						})
					end
					vim.api.nvim_create_autocmd("FileType", {
						group = "dotfiles-settings",
						pattern = "scss",
						callback = require("colorizer/sass").attach_to_buffer,
					})
				end,
			}, -- Highlight colors in files
			{
				"rebelot/heirline.nvim",
				cond = require("dotfiles.utils.use_termguicolors"),
			}, -- Statusline
		})
	end,
})
