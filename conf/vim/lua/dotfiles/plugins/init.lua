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
				-- Configured in ~/dotfiles/conf/vim/after/plugin/lexima.lua
			}, -- Autopairs + Endwise
			"michaeljsmith/vim-indent-object", -- ii, ai, aI for indent-based textobjects
			-- Extra functionality + UI:
			{ "kyazdani42/nvim-web-devicons", cond = require("dotfiles.utils.use_termguicolors") }, -- Icons, used in the statusline
			{
				"ibhagwan/fzf-lua",
				-- Configured in ~/dotfiles/conf/vim/after/plugin/fzf-lua.lua
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
				-- Configured in ~/dotfiles/conf/vim/after/plugin/luasnip.lua
				requires = {
					{ "rafamadriz/friendly-snippets", after = { "LuaSnip" } }, -- Base Snippets
					{ "edheltzel/vscode-jekyll-snippets", ft = { "markdown", "html" } }, -- Jekyll Snippets
				},
			}, -- Snippets
			{
				"jose-elias-alvarez/null-ls.nvim",
				-- Configured in ~/dotfiles/conf/vim/after/plugin/null-ls.lua
				requires = { { "nvim-lua/plenary.nvim", module = "plenary" } },
			},
			{
				"neovim/nvim-lspconfig",
				requires = {
					{ "williamboman/nvim-lsp-installer", module = "nvim-lsp-installer" },
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
					local servers = require("dotfiles.nvim-lsp.servers")
					local on_attach = require("dotfiles.nvim-lsp.on_attach")

					vim.diagnostic.config({
						underline = true,
						virtual_text = true,
						signs = false,
						severity_sort = true,
					})

					-- LSP Logging:
					-- vim.lsp.set_log_level("trace")

					local handler_no_diagnostics = {
						["textDocument/publishDiagnostics"] = function() end,
					}
					local capabilities = vim.lsp.protocol.make_client_capabilities()

					require("nvim-lsp-installer").setup({
						ensure_installed = vim.tbl_keys(servers),
					})

					for lsp, settings in pairs(servers) do
						local opts = {
							on_attach = on_attach,
							capabilities = capabilities,
						}
						if #vim.tbl_keys(settings) > 0 then
							opts = vim.tbl_extend("keep", opts, settings)
						end
						if not vim.tbl_contains(servers[lsp].provides or {}, "diagnostics") then
							opts.handlers = handler_no_diagnostics
						end
						if lsp ~= "null-ls" then
							require("lspconfig")[lsp].setup(opts)
						end
					end
				end,
			}, -- LSP
			{
				"anuvyklack/hydra.nvim",
				config = function()
				end,
			},
			{
				"nvim-treesitter/nvim-treesitter",
				-- Configured in ~/dotfiles/conf/vim/after/plugin/nvim-treesitter.lua
				run = function()
					vim.cmd([[TSUpdate]])
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
			-- {
			-- 	"lukas-reineke/indent-blankline.nvim",
			-- 	config = function()
			-- 		require("indent_blankline").setup({
			-- 			buftype_exclude = { "terminal" },
			-- 			filetype_exclude = {
			-- 				"diff",
			-- 				"fzf",
			-- 				"gina-status",
			-- 				"help",
			-- 				"lsp-installer",
			-- 				"markdown",
			-- 				"packer",
			-- 				"qf",
			-- 			},
			-- 			show_current_context = true,
			-- 			context_patterns = { "class", "function", "method", "^if", "table", "^for", "^while" },
			-- 			show_current_context_start = true,
			-- 		})
			-- 	end,
			-- },
			{
				"oncomouse/lushwal.nvim",
				requires = { { "rktjmp/lush.nvim", opt = true }, { "rktjmp/shipwright.nvim", opt = true } },
				config = function()
					vim.g.lushwal_configuration = {
						addons = {
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
		})
	end,
})
