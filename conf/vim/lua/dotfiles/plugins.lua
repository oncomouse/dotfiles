--luacheck: globals vim dotfiles
dotfiles = _G.dotfiles or {}
return require("packer").startup({
	function(use)
		use({ "wbthomason/packer.nvim", opt = true })
		use({ "dstein64/vim-startuptime", cmd = "StartupTime" })
		use("tpope/vim-sensible")
		use("xero/securemodelines")
		use("sickill/vim-pasta") -- fix block paste for Neovim
		use("tpope/vim-commentary") -- gc<motion> to (un)comment
		use("tpope/vim-repeat")
		use("oncomouse/vim-surround") -- ys to add, cs to change, ds to delete. f, F for function, t, T for tag
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
		use("airblade/vim-rooter") -- Set CWD for projects
		use("wellle/targets.vim") -- add next block n]) targets, plus words in commas (a,), asterisks (a*), etc
		use("cohama/lexima.vim") -- Autopairs + Endwise
		use({
			"norcalli/nvim-colorizer.lua",
			event = "BufReadPre",
			config = function()
				if vim.opt.termguicolors:get() then
					require("colorizer").setup({
						["*"] = {},
						markdown = { names = false },
						text = { names = false },
						["gina-commit"] = { names = false },
						css = { css = true },
						scss = { css = true },
					})
				end
			end,
		})
		use("junegunn/fzf.vim") -- Add shorcuts for FZF
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
		use({
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
				vim.fn["gina#custom#mapping#nmap"]("status", "cc", ":<C-u>Gina commit<CR>", { noremap = 1, silent = 1 })
			end,
		})

		-- Treesitter:
		use({
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
				})
			end,
			requires = {
				{ "nvim-treesitter/nvim-treesitter-textobjects" },
				{ "windwp/nvim-ts-autotag", ft = { "html", "javascript", "javascriptreact" } },
			},
		})

		-- Non-Treesitter Syntax:
		use({
			"plasticboy/vim-markdown",
			ft = "markdown",
		}) -- Markdown Syntax
		vim.g.vim_markdown_frontmatter = 1 -- Format YAML
		vim.g.vim_markdown_strikethrough = 0 -- Don't format strikethrough
		vim.g.vim_markdown_conceal = 0 -- Don't conceal
		vim.g.vim_markdown_conceal_code_blocks = 0 -- Don't conceal code blocks
		vim.g.vim_markdown_math = 1 -- Do process MathJaX and LaTeX math
		use({ -- Required for TableFormat in vim-markdown but also useful elsewhere
			"godlygeek/tabular",
			cmd = { "Tabularize" },
		})
		use({
			"elkowar/yuck.vim",
		})

		-- Snippets:
		use({
			"hrsh7th/vim-vsnip",
			event = "VimEnter",
			config = function()
				local map = require("dotfiles.utils.map")
				vim.g.vsnip_snippet_dir = os.getenv("HOME") .. "/dotfiles/conf/vim/snippets"
				vim.opt.completefunc = "vsnip_completefunc#completefunc"
				map.imap("<expr>", "<Tab>", "vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : '<Tab>'")
				map.smap("<expr>", "<Tab>", "vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : '<Tab>'")
				map.imap("<expr>", "<S-Tab>", "vsnip#jumpable(-11) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'")
				map.smap("<expr>", "<S-Tab>", "vsnip#jumpable(-(1) ? '<Plug>(vsnip-jump-prev)' : '<S-Tab>'")
			end,
			requires = {
				{ "rafamadriz/friendly-snippets", after = { "vim-vsnip" } }, -- Base Snippets
				{ "edheltzel/vscode-jekyll-snippets", ft = { "markdown", "html" } }, -- Jekyll Snippets
			},
		})

		-- LSP:
		local lsp_types = {
			"css",
			"fish",
			"html",
			"javascript",
			"json",
			"jsonc",
			"lua",
			"python",
			"ruby",
			"scss",
			"sh",
			"vim",
			"yaml",
		}
		use({
			"williamboman/nvim-lsp-installer",
			requires = {
				{ "neovim/nvim-lspconfig" },
				{ "hrsh7th/vim-vsnip-integ", opt = true, requires = { "vim-vsnip" } },
				{ "nvim-lua/plenary.nvim", opt = true },
				{ "jose-elias-alvarez/null-ls.nvim", opt = true },
			},
			ft = lsp_types,
			config = function()
				require("dotfiles.nvim_lsp")
			end,
		})
	end,
})
