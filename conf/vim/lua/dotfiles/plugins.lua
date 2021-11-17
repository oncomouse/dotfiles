--luacheck: globals vim dotfiles
dotfiles = _G.dotfiles or {}
return require("packer").startup({
	function(use)
		use({
			{ "wbthomason/packer.nvim", opt = true },
			{ "dstein64/vim-startuptime", cmd = "StartupTime" },
			"sickill/vim-pasta", -- fix block paste for Neovim
			"tpope/vim-commentary", -- gc<motion> to (un)comment
			"tpope/vim-repeat",
			"oncomouse/vim-surround", -- ys to add, cs to change, ds to delete. f, F for function, t, T for tag
			"wellle/targets.vim", -- add next block n]) targets, plus words in commas (a,), asterisks (a*), etc
			{
				"airblade/vim-rooter",
				event = "VimEnter",
				setup = function()
					vim.opt.path = ",,"
					vim.g.rooter_patterns = {
						"Rakefile",
						"package.json",
						".git/",
						"Gemfile",
						"pyproject.toml",
						"setup.py",
						"Makefile",
					}
				end,
			}, -- Set CWD for projects
			-- {
			-- 	"cohama/lexima.vim",
			-- 	config = function()
			-- 		vim.cmd([[autocmd! dotfiles-settings FileType lua call dotfiles#lexima#extend_endwise()]])
			-- 		local map = require("dotfiles.utils.map")
			-- 		map.inoremap("<silent>", "<Plug>(dotfiles-lexima)", '<C-r>=lexima#insmode#leave_till_eol("")<CR>')
			-- 		map.imap("<silent>", "<C-l>", "<Plug>(dotfiles-lexima)")
			-- 	end,
			-- }, -- Autopairs + Endwise
			{
				"windwp/nvim-autopairs",
				config = function()
					local npairs = require("nvim-autopairs")
					local endwise = require("nvim-autopairs.ts-rule").endwise
					npairs.setup({
						fast_wrap = {},
					})
					npairs.add_rules(require("nvim-autopairs.rules.endwise-lua"))
					npairs.add_rules(require("nvim-autopairs.rules.endwise-ruby"))
					-- Why no loops in nvim-autopairs builtins?
					npairs.add_rules({
						endwise("do$", "end", "lua", {
							"for_in_statement",
							"while_statement",
						}),
					})
					-- VimL rules from lexima.vim
					local vim_rules = {}
					for _, at in ipairs({
						"fu",
						"fun",
						"func",
						"funct",
						"functi",
						"functio",
						"function",
						"if",
						"wh",
						"whi",
						"whil",
						"while",
						"for",
						"try",
					}) do
						table.insert(vim_rules, endwise("^%s*" .. at .. ".*$", "end" .. at, "vim", {}))
					end
					for _, at in ipairs({ "aug", "augroup" }) do
						table.insert(vim_rules, endwise("^%s*" .. at .. "%s+.+$", at .. " END", "vim", {}))
					end
					npairs.add_rules(vim_rules)
					-- Shell rules:
					npairs.add_rules({
						endwise("^%s*if.*$", "fi", { "sh", "zsh" }, {}),
						endwise("^%s*case.*$", "esac", { "sh", "zsh" }, {}),
						endwise("^%s*if.*$", "fi", { "sh", "zsh" }, {}),
						endwise("%sdo$", "done", { "sh", "zsh" }, {}),
					})
				end,
			},
			{
				"norcalli/nvim-colorizer.lua",
				config = function()
					if vim.opt.termguicolors:get() then
						require("colorizer").setup({
							["*"] = { names = false, RRGGBBAA = true },
							packer = { RGB = false },
							html = { names = true, RRGGBBAA = false },
							css = { css = true, RRGGBBAA = false },
							scss = { css = true, RRGGBBAA = false },
						})
					end
				end,
			},
			{
				"junegunn/fzf.vim",
				event = "VimEnter",
				setup = function()
					vim.cmd(
						[[command! -bang -nargs=? -complete=dir Files call fzf#vim#files(<q-args>, fzf#vim#with_preview({'options': ['--reverse', '--info=inline']}), <bang>0)]]
					)
					vim.g.fzf_layout = { window = { width = 1, height = 0.4, yoffset = 1, border = "top" } }
					vim.g.fzf_action = {
						["ctrl-s"] = "split",
						["ctrl-v"] = "vsplit",
						["ctrl-t"] = "tabnew",
						["ctrl-e"] = "edit",
					}
					vim.g.fzf_nvim_statusline = 0 -- disable statusline overwriting
				end,
			}, -- Add shorcuts for FZF
			{
				"lambdalisue/gina.vim",
				cmd = "Gina",
				config = function()
					vim.fn["gina#custom#command#option"]("status", "--opener", vim.opt.previewheight:get() .. "split")
					vim.fn["gina#custom#command#option"]("commit", "--opener", vim.opt.previewheight:get() .. "split")
					vim.fn["gina#custom#command#option"]("diff", "--opener", vim.opt.previewheight:get() .. "split")
					vim.fn["gina#custom#command#option"]("status", "--group", "short")
					vim.fn["gina#custom#command#option"]("commit", "--group", "short")
					vim.fn["gina#custom#command#option"]("diff", "--group", "short")
					-- Implement vim-fugitive commands in Gina:
					vim.fn["gina#custom#mapping#nmap"]("status", "cc", ":<C-u>Gina commit<CR>", {
						noremap = 1,
						silent = 1,
					})
				end,
			}, -- Git support
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
				end,
				requires = {
					{ "rafamadriz/friendly-snippets", after = { "vim-vsnip" } }, -- Base Snippets
					{ "edheltzel/vscode-jekyll-snippets", ft = { "markdown", "html" } }, -- Jekyll Snippets
				},
			}, -- Snippets
			{
				"neovim/nvim-lspconfig",
				requires = {
					{ "williamboman/nvim-lsp-installer", module = "nvim-lsp-installer" },
					{ "hrsh7th/vim-vsnip-integ", opt = true, requires = { "vim-vsnip" } },
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
					"rust",
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
						indent = { enable = true },
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
							config = {
								vim = {
									__default = '" %s',
									lua_statement = "-- %s",
								},
							},
						},
						playground = {
							enable = true,
							disable = {},
							updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
							persist_queries = false, -- Whether the query persists across vim sessions
							keybindings = {
								toggle_query_editor = "o",
								toggle_hl_groups = "i",
								toggle_injected_languages = "t",
								toggle_anonymous_nodes = "a",
								toggle_language_display = "I",
								focus_language = "f",
								unfocus_language = "F",
								update = "R",
								goto_node = "<cr>",
								show_help = "?",
							},
						},
					})
				end,
				requires = {
					"nvim-treesitter/playground",
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
					vim.g.vim_markdown_strikethrough = 0 -- Don't format strikethrough
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
				"lukas-reineke/indent-blankline.nvim",
				config = function()
					local status, wal_colors = pcall(require, "dotfiles.wal-colors")
					if not status then
						wal_colors = {
							color8 = "#000000",
							color7 = "#FFFFFF",
						}
					end
					-- Indent character color:
					vim.cmd(
						"highlight IndentBlanklineChar guifg="
							.. wal_colors.color8
							.. " gui=nocombine ctermfg=8 cterm=nocombine"
					)
					-- Context indent character color:
					vim.cmd(
						"highlight IndentBlanklineContextChar guifg="
							.. wal_colors.color7
							.. " gui=nocombine ctermfg=7 cterm=nocombine"
					)
					-- Start of context underline color:
					vim.cmd(
						"highlight IndentBlanklineContextStart guisp="
							.. wal_colors.color7
							.. " gui=underline cterm=underline"
					)
					require("indent_blankline").setup({
						buftype_exclude = { "terminal" },
						filetype_exclude = { "diff", "gina-status", "help", "markdown", "packer", "qf" },
						show_current_context = true,
						context_patterns = { "class", "function", "method", "^if", "table", "^for", "^while" },
						show_current_context_start = true,
					})
				end,
			},
		})
	end,
})
