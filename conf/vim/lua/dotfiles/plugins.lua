_dotfiles = _dotfiles or {}
vim.g.dotfiles_ts_playground = true -- Load playground
return require("packer").startup({
	function(use)
		use({
			{ "wbthomason/packer.nvim", opt = true },
			-- {
			-- 	"oncomouse/plenary-script.nvim",
			-- 	requires = { "nvim-lua/plenary.nvim" },
			-- }, -- Content-based filetype detection using plenary.nvim
			"sickill/vim-pasta", -- fix block paste for Neovim
			"tpope/vim-commentary", -- gc<motion> to (un)comment
			{ "oncomouse/vim-surround", requires = { "tpope/vim-repeat" } }, -- ys to add, cs to change, ds to delete. f, F for function, t, T for tag
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
			"oncomouse/vim-lion", -- gl and gL to align
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
			}, -- Fancy * and # bindings
			"vim-scripts/ReplaceWithRegister", -- gr{motion} or grr or gr in visual to replace with register
			{
				"cohama/lexima.vim",
				config = function()
					local function make_rule(at, ed, ft, syn)
						return {
							char = "<CR>",
							input = "<CR>",
							input_after = "<CR>" .. ed,
							at = at,
							except = [[\C\v^(\s*)\S.*%#\n%(%(\s*|\1\s.+)\n)*\1]] .. ed,
							filetype = ft,
							syntax = syn,
						}
					end
					vim.api.nvim_create_autocmd("FileType", {
						pattern = "lua",
						group = "dotfiles-settings",
						callback = function()
							vim.fn["lexima#add_rule"](
								make_rule([[^\s*if\>.*then\%(.*[^.:@$]\<end\>\)\@!.*\%#]], "end", "lua", {})
							)
							vim.fn["lexima#add_rule"](
								make_rule([[^\s*\%(for\|while\)\>.*do\%(.*[^.:@$]\<end\>\)\@!.*\%#]], "end", "lua", {})
							)
							vim.fn["lexima#add_rule"](
								make_rule(
									[[^\s*\%(local\)\=.*function\>\%(.*[^.:@$]\<end\>\)\@!.*\%#]],
									"end",
									"lua",
									{}
								)
							)
						end,
					})
					vim.keymap.set(
						"i",
						"<Plug>(dotfiles-lexima)",
						'<C-r>=lexima#insmode#leave_till_eol("")<CR>',
						{ noremap = true }
					)
					vim.keymap.set("i", "<C-l>", "<Plug>(dotfiles-lexima)", { silent = true })
				end,
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
					for _, command in
						pairs({
							"branch",
							"changes",
							"commit",
							"diff",
							"log",
							"status",
						})
					do
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
				"phaazon/hop.nvim",
				keys = { "<leader>f", "<leader>F", "<localleader>f", "<localleader>F" },
				config = function()
					require("hop").setup({})
					local maps = {
						{ "<leader>f", "<cmd>HopChar1AC<cr>" },
						{ "<leader>F", "<cmd>HopChar1BC<cr>" },
						{ "<localleader>f", "<cmd>HopChar2AC<cr>" },
						{ "<localleader>F", "<cmd>HopChar2BC<cr>" },
					}
					for _, mapdef in pairs(maps) do
						vim.keymap.set({ "n", "o", "x" }, mapdef[1], mapdef[2])
					end
				end,
			}, -- Fancy jump, useful for text editing
			{
				"hrsh7th/vim-vsnip",
				event = "VimEnter",
				config = function()
					vim.g.vsnip_snippet_dir = os.getenv("HOME") .. "/dotfiles/conf/vim/snippets"
					vim.keymap.set(
						{ "s", "i" },
						"<Tab>",
						"vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : '<Tab>'",
						{ expr = true, remap = true }
					)
					vim.keymap.set(
						{ "s", "i" },
						"<S-Tab>",
						"vsnip#jumpable(-1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'",
						{ expr = true, remap = true }
					)
					vim.api.nvim_create_autocmd("CompleteDone", {
						group = "dotfiles-settings",
						command = "if vsnip#available(1) | call vsnip#expand() | endif",
					})
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
					})
				end,
				requires = {
					{
						"nvim-treesitter/nvim-treesitter-textobjects",
						config = function()
							require("nvim-treesitter.configs").setup({
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
							})
						end,
					},
					{
						"windwp/nvim-ts-autotag",
						ft = { "html", "javascript", "javascriptreact" },
						config = function()
							require("nvim-treesitter.configs").setup({
								autotag = { enable = true },
							})
						end,
					},
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
				"sukima/xmledit",
				ft = "xml",
				setup = function()
					vim.g.xml_no_comment_map = 1
				end,
			}, -- XML tag close
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
						callback = function()
							require("colorizer/sass").attach_to_buffer()
						end,
					})
				end,
			}, -- Highlight colors in files
			{
				"rebelot/heirline.nvim",
				config = function()
					require("heirline").setup(require("dotfiles.heirline"))
				end,
				cond = require("dotfiles.utils.use_termguicolors"),
			}, -- Statusline
		})
	end,
})
