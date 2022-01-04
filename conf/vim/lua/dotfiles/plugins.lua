--luacheck: globals vim dotfiles
dotfiles = _G.dotfiles or {}
vim.g.dotfiles_ts_playground = true -- Load playground
return require("packer").startup({
	function(use)
		use({
			{ "wbthomason/packer.nvim", opt = true },
			"sickill/vim-pasta", -- fix block paste for Neovim
			"tpope/vim-commentary", -- gc<motion> to (un)comment
			{ "oncomouse/vim-surround", requires = { "tpope/vim-repeat" } }, -- ys to add, cs to change, ds to delete. f, F for function, t, T for tag
			{ "wellle/targets.vim", requires = { "tpope/vim-repeat" } }, -- add next block n]) targets, plus words in commas (a,), asterisks (a*), etc
			{
				"monkoose/matchparen.nvim",
				config = function()
					require("matchparen").setup()
				end,
			}, -- Faster matchparen for Neovim
			"oncomouse/vim-lion", -- gl and gL for alignment
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
			{
				"windwp/nvim-autopairs",
				config = function()
					require("dotfiles.autopairs.fast-wrap")
					require("dotfiles.autopairs.endwise-ruby")
					require("dotfiles.autopairs.endwise-lua")
					require("dotfiles.autopairs.endwise-vim")
					require("dotfiles.autopairs.endwise-sh")
					require("dotfiles.autopairs.rules-markdown")

					-- Configuration for cmp:
					local cmp_autopairs = require("nvim-autopairs.completion.cmp")
					local cmp = require("cmp")
					cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done())

					-- Jump closed autopairs:
					local map = require("dotfiles.utils.map")
					map.imap("<C-L>", require("dotfiles.jump-autopairs"))
				end,
				after = "nvim-cmp",
			}, -- Autopairs + Endwise
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
						},
						files = {
							actions = {
								["default"] = require("fzf-lua.actions").file_edit,
							},
						},
					})
					vim.api.nvim_add_user_command("Files", function(args)
						require("fzf-lua").files({
							fzf_opts = { ["--layout"] = "reverse-list", ["--info"] = "inline" },
							cwd = args.args == "" and "." or args.args,
						})
					end, {
						complete = "dir",
						force = true,
						nargs = "?",
					})
					vim.api.nvim_add_user_command("Buffers", function()
						require("fzf-lua").buffers()
					end, {
						force = true,
					})
					vim.api.nvim_add_user_command("GitStatus", function()
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
					for _,command in pairs({
						"branch",
						"changes",
						"commit",
						"diff",
						"log",
						"status",
					}) do
						vim.fn["gina#custom#command#option"](command, "--opener", vim.opt.previewheight:get() .. "split")
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
				"phaazon/hop.nvim",
				config = function()
					require("hop").setup({})
					local map = require("dotfiles.utils.map")
					map.nmap("<leader>f", "<cmd>HopChar1AC<cr>")
					map.nmap("<leader>F", "<cmd>HopChar1BC<cr>")
					map.omap("<leader>f", "<cmd>HopChar1AC<cr>")
					map.omap("<leader>F", "<cmd>HopChar1BC<cr>")
					map.nmap("<localleader>f", "<cmd>HopChar2AC<cr>")
					map.nmap("<localleader>F", "<cmd>HopChar2BC<cr>")
					map.omap("<localleader>f", "<cmd>HopChar2AC<cr>")
					map.omap("<localleader>F", "<cmd>HopChar2BC<cr>")
				end,
			}, -- Fancy jump, useful for text editing
			{
				"hrsh7th/vim-vsnip",
				event = "VimEnter",
				config = function()
					local map = require("dotfiles.utils.map")
					vim.g.vsnip_snippet_dir = os.getenv("HOME") .. "/dotfiles/conf/vim/snippets"
					map.imap("<expr>", "<Tab>", "vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : '<Tab>'")
					map.smap("<expr>", "<Tab>", "vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : '<Tab>'")
					map.imap("<expr>", "<S-Tab>", "vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'")
					map.smap("<expr>", "<S-Tab>", "vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'")
					vim.cmd(
						"autocmd dotfiles-settings CompleteDone * if vsnip#available(1) | call vsnip#expand() | endif"
					)
				end,
				requires = {
					{ "rafamadriz/friendly-snippets", after = { "vim-vsnip" } }, -- Base Snippets
					{ "edheltzel/vscode-jekyll-snippets", ft = { "markdown", "html" } }, -- Jekyll Snippets
				},
			}, -- Snippets
			{
				"hrsh7th/nvim-cmp",
				requires = {
					"hrsh7th/cmp-nvim-lsp",
					"hrsh7th/cmp-vsnip",
					"hrsh7th/cmp-path",
				},
				config = function()
					local cmp = require("cmp")
					cmp.setup({
						snippet = {
							expand = function(args)
								vim.fn["vsnip#anonymous"](args.body)
							end,
						},
						mapping = {
							["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
							["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
							["<C-d>"] = cmp.mapping.scroll_docs(-4),
							["<C-f>"] = cmp.mapping.scroll_docs(4),
							["<C-x><C-o>"] = cmp.mapping.complete(),
							["<C-c>"] = cmp.mapping.abort(),
							["<C-e>"] = cmp.mapping.close(),
							["<C-y>"] = cmp.mapping.confirm({ select = true }),
							["<Tab>"] = cmp.mapping.confirm({ select = true }),
						},
						sources = {
							{ name = "nvim_lsp" },
							{ name = "vsnip" },
							{ name = "path" },
						},
						completion = {
							autocomplete = false,
						},
					})
				end,
			}, -- Completion
			{
				"neovim/nvim-lspconfig",
				requires = {
					{ "williamboman/nvim-lsp-installer", module = "nvim-lsp-installer" },
					{
						"jose-elias-alvarez/null-ls.nvim",
						module = "null-ls",
						requires = { { "nvim-lua/plenary.nvim", module = "plenary" } },
					},
					{
						"jose-elias-alvarez/nvim-lsp-ts-utils",
						ft = { "javascript", "javascriptreact", "typescript", "typescriptreact" },
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
					require("dotfiles.nvim_lsp")
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
						ensure_installed = "maintained",
						highlight = { enable = true },
						autotag = { enable = true },
						textobjects = {
							select = {
								enable = true,
								lookahead = true,
								keymaps = {
									["af"] = "@function.outer",
									["if"] = "@function.inner",
									["ac"] = "@class.outer",
									["ic"] = "@class.inner",
								},
							},
						},
						context_commentstring = {
							enable = true,
						},
					})
				end,
				requires = {
					{ "nvim-treesitter/nvim-treesitter-textobjects" },
					{ "windwp/nvim-ts-autotag", ft = { "html", "javascript", "javascriptreact" } },
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
			{
				"Fymyte/rasi.vim",
				ft = "rasi",
			}, -- Rasi Syntax for Rofi
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
			},
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
					vim.cmd([[autocmd dotfiles-settings FileType scss lua require'colorizer/sass'.attach_to_buffer()]])
				end,
			},
		})
	end,
})
