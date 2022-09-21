local xdg = require("dotfiles.utils.xdg")
local packer_path = xdg("XDG_DATA_HOME") .. "/packer.nvim"
local pack_path = packer_path .. "/pack"
local install_path = pack_path .. "/packer/opt/packer.nvim"
local compile_path = packer_path .. "/plugin"

local function plugins()
	pcall(vim.cmd, [[packadd packer.nvim]])
	local ok, util = pcall(require, "packer.util")

	if not ok then
		return
	end

	return require("packer").startup({
		function(use, use_rocks)
			use_rocks({
				{ "lpeg-bibtex", server = "https://luarocks.org/dev" },
			})
			use({
				{ "wbthomason/packer.nvim", opt = true },

				-- Editor Enhancements:

				"sickill/vim-pasta", -- fix block paste for Neovim
				"christoomey/vim-sort-motion", -- gs to sort
				"tpope/vim-sleuth", -- Automatically set indent
				"oncomouse/vim-lion", -- gl and gL to align

				{
					"vim-scripts/ReplaceWithRegister",
					requires = { "tpope/vim-repeat" },
				}, -- gr{motion} or grr or gr in visual to replace with register
				{
					"tpope/vim-unimpaired",
					requires = { "tpope/vim-repeat" },
				},

				{
					"echasnovski/mini.nvim",
					module = {
						"mini.ai",
						"mini.base16",
						"mini.bufremove",
						"mini.comment",
						"mini.completion",
						"mini.cursorword",
						"mini.doc",
						"mini.fuzzy",
						"mini.indentscope",
						"mini.jump",
						"mini.jump2d",
						"mini.misc",
						"mini.pairs",
						"mini.sessions",
						"mini.starter",
						"mini.statusline",
						"mini.surround",
						"mini.tabline",
						"mini.test",
						"mini.trailspace",
					},
					requires = {
						{ "preservim/vim-textobj-sentence", after = "mini.nvim" }, -- Sentence object
						{ "nvim-treesitter/nvim-treesitter-textobjects", after = "nvim-treesitter" }, -- Configuration for treesitter objects
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
						vim.keymap.set("", "*", "<Plug>(asterisk-*)")
						vim.keymap.set("", "#", "<Plug>(asterisk-#)")
						vim.keymap.set("", "g*", "<Plug>(asterisk-g*)")
						vim.keymap.set("", "g#", "<Plug>(asterisk-g#)")
						vim.g["asterisk#keeppos"] = 1
					end,
					requires = { "tpope/vim-repeat" },
				}, -- Fancy * and # bindings

				{
					"cohama/lexima.vim", -- Autopairs
					setup = function()
						vim.g.lexima_enable_space_rules = 0
						vim.g.lexima_enable_endwise_rules = 1
						vim.g.lexima_disable_closetag = 0
						vim.g.lexima_no_default_rules = 1
					end,
					event = "InsertEnter",
					config = function()
						require("dotfiles.plugins.lexima").setup()
						-- Autoclose mapping:
						vim.keymap.set("i", "<C-l>", "<Plug>(dotfiles-lexima)", { silent = true })
					end,
					-- config = require("dotfiles.plugins.lexima"),
					-- Configured in ~/dotfiles/conf/vim/after/plugin/lexima.lua
				}, -- Endwise and autopairs

				-- Extra functionality + UI:

				{
					"kyazdani42/nvim-web-devicons",
					cond = require("dotfiles.utils.use_termguicolors"),
					module = "nvim-web-devicons",
				}, -- Icons, used in the statusline

				{
					"ibhagwan/fzf-lua",
					keys = { { "n", "<C-p>" }, { "n", "<Leader>a" } },
					cmd = { "FzfLua", "Files", "Buffers", "GitStatus" },
					module = "fzf-lua",
					setup = function() -- Shim vim.ui.select until we can load the plugin
						vim.ui.select = function(...)
							require("fzf-lua.providers.ui_select").ui_select(...)
						end
					end,
					config = function()
						require("dotfiles.plugins.fzf-lua").setup()
						vim.keymap.set("n", "<C-p>", "<Plug>(dotfiles-fzf-files)", { silent = true })
						vim.keymap.set("n", "<Leader>a", "<Plug>(dotfiles-fzf-buffers)", { silent = true })
					end,
					-- Configured in ~/dotfiles/conf/vim/lua/dotfiles/plugins/fzf-lua.lua
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
					config = require("dotfiles.plugins.luasnip"),
					event = "InsertEnter",
					requires = {
						{
							"rafamadriz/friendly-snippets",
						}, -- Base Snippets
					},
					-- Configured in ~/dotfiles/conf/vim/lua/dotfiles/plugins/luasnip.lua
				}, -- Snippets

				{
					"jose-elias-alvarez/null-ls.nvim",
					requires = {
						{ "nvim-lua/plenary.nvim", module = { "plenary.async", "plenary" } },
						{ "williamboman/mason.nvim", module = "mason" },
					},
					config = require("dotfiles.plugins.null-ls"),
					module = "null-ls", -- Null-ls is loaded by the events created in nvim-lspconfig's configuration
					-- Configured in ~/dotfiles/conf/vim/lua/dotfiles/plugins/null-ls/init.lua
				}, -- Format and Diagnostics

				{
					"neovim/nvim-lspconfig",
					requires = {
						{ "williamboman/mason.nvim", module = "mason" },
						{ "williamboman/mason-lspconfig.nvim", module = "mason-lspconfig" },
					},
					-- Configured in ~/dotfiles/conf/vim/plugin/nvim-lspconfig.lua
				}, -- LSP

				{
					"hrsh7th/nvim-cmp",
					module = "cmp",
					event = "InsertEnter",
					after = { "LuaSnip" },
					requires = {
						{ "hrsh7th/cmp-nvim-lsp" },
						{ "saadparwaiz1/cmp_luasnip" },
						{ "hrsh7th/cmp-nvim-lua" },
						{ "mtoohey31/cmp-fish", ft = "fish" },
					},
					config = require("dotfiles.plugins.cmp"),
					-- Configured in ~/dotfiles/conf/vim/lua/dotfiles/plugins/cmp.lua
				}, -- Completion (used as a super omnifunc (C-X C-O))

				{
					"anuvyklack/hydra.nvim",
					config = function()
						require("dotfiles.plugins.hydra").setup()
					end,
					-- Configured in ~/dotfiles/conf/vim/plugin/hydra.lua
				}, -- Repeating keys mode (used for window resizing, atm)

				{
					"gaoDean/autolist.nvim",
					ft = { "markdown", "text" },
					config = function()
						require("autolist").setup({
							colon = {
								preferred = "",
								indent = false,
								indent_raw = false,
							},
							invert = {
								ul_marker = "*",
							},
							insert_mappings = {
								invert = {
									"",
								},
								indent = {
									"",
								},
							},
						})
						-- <C-d> to delete list marker if that's all that's left
						vim.api.nvim_create_autocmd("FileType", {
							pattern = "markdown,text",
							group = "dotfiles-settings",
							callback = function(args)
								vim.keymap.set("i", "<C-d>", function()
									local line = vim.api.nvim_get_current_line()
									-- Check line for unordered list:
									local match = string.match(line, "^[*-] ")
									-- Check line for ordered list:
									if not match then
										match = string.match(line, "^%d+%. ")
									end
									if match then
										local savepos = vim.fn.winsaveview().col
										local jump = (savepos == #line) and "$a" or savepos - #match .. "li"
										return '<Esc>0"_2dl' .. jump
									end
									return "<C-d><Cmd>lua require('autolist').detab()<CR>"
								end, {
									expr = true,
									buffer = args.buf,
								})
							end,
						})
					end,
				}, -- Auto-increment and indent/dedent lists

				{
					"nvim-treesitter/nvim-treesitter",
					run = function()
						vim.cmd([[TSUpdate]])
					end,
					requires = {
						{
							"JoosepAlviste/nvim-ts-context-commentstring", -- Contextual commentstring
							after = { "nvim-treesitter", "mini.nvim" },
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
						{
							"andymass/vim-matchup",
							after = "nvim-treesitter",
							setup = function()
								vim.g.matchup_matchparen_offscreen = {
									method = "popup",
								}
							end,
						},
					},
					-- Configured in ~/dotfiles/conf/vim/plugin/nvim-treesitter.lua
				}, -- Treesitter-based Syntax

				-- Appearance:

				{
					"preservim/vim-markdown",
					ft = "markdown",
					setup = function()
						vim.g.vim_markdown_frontmatter = 1 -- Format YAML
						vim.g.vim_markdown_strikethrough = 1 -- Don't format strikethrough
						vim.g.vim_markdown_conceal = 0 -- Don't conceal
						vim.g.vim_markdown_conceal_code_blocks = 0 -- Don't conceal code blocks
						vim.g.vim_markdown_math = 1 -- Do process MathJaX and LaTeX math
						vim.g.vim_markdown_auto_insert_bullets = 0 -- autoList handles bullet insertion now
						vim.g.vim_markdown_new_list_item_indent = 0 -- autoList handles spacing for lists
					end,
					requires = {
						{ -- Required for TableFormat in vim-markdown but also useful elsewhere
							"godlygeek/tabular",
							cmd = { "Tabularize" },
						},
					},
				}, -- Markdown Syntax

				{
					"oncomouse/lushwal.nvim",
					opt = true,
					cmd = "LushwalCompile",
					setup = function()
						vim.g.lushwal_configuration = {
							-- Ayu Mirage is weird, so we do some overrides:
							color_overrides = function(colors)
								local overrides = {
									red = colors.color5,
									orange = colors.color1,
									amaranth = colors.color5.mix(colors.color4, 34).saturate(46).darken(5),
								}
								return vim.tbl_extend("force", colors, overrides)
							end,
							-- Here's all the addons we need:
							addons = {
								hydra_nvim = true,
								indent_blankline_nvim = true,
								gina = true,
								markdown = true,
								mini_nvim = true,
								nvim_cmp = true,
							},
						}
						vim.api.nvim_create_autocmd("ColorSchemePre", {
							group = "dotfiles-settings",
							pattern = "lushwal",
							command = "PackerLoad lushwal.nvim",
						})
					end,
					run = function()
						vim.cmd([[LushwalCompile]])
					end,
					requires = { { "rktjmp/lush.nvim", opt = true }, { "rktjmp/shipwright.nvim", opt = true } },
				}, -- Colorscheme

				"NvChad/nvim-colorizer.lua", -- Highlight colors in files
				-- Configured in ~/dotfiles/conf/vim/plugin/nvim-colorizer.lua

				{
					"rebelot/heirline.nvim",
					module = "heirline",
				}, -- Statusline
				-- Configured in ~/dotfiles/conf/vim/after/plugin/heirline.lua

				{
					"lukas-reineke/indent-blankline.nvim",
					module = "indent_blankline",
				}, -- Mark and highlight indentations
				-- Configured in ~/dotfiles/conf/vim/after/plugin/indent-blankline.lua
			})
		end,
		config = {
			package_root = pack_path,
			compile_path = util.join_paths(compile_path, "packer_compiled.lua"),
		},
	})
end

vim.opt.runtimepath:append(packer_path)
vim.opt.packpath:append(packer_path)

local packer_commands = {
	"install",
	"update",
	"sync",
	"clean",
	"status",
	"compile",
}
for _, cmd in pairs(packer_commands) do
	vim.api.nvim_create_user_command("Packer" .. cmd:gsub("^%l", string.upper), function()
		plugins()[cmd]()
	end, {})
end
vim.api.nvim_create_user_command("PackerLoad", function(args)
	plugins().loader(args.args, args.bang)
end, {
	nargs = "+",
	bang = true,
})

-- Since the autocommand is causing problems, let's run a command:
vim.api.nvim_create_user_command("DotfilesCompile", "so % | PackerCompile", {})

-- Install packer.nvim, if it isn't present:
if vim.fn.empty(vim.fn.glob(install_path)) == 1 then
	vim.fn.jobstart({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path }, {
		on_exit = function()
			plugins().sync()
		end,
	})
end

return plugins
