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
		function(use)
			use({
				{ "wbthomason/packer.nvim", opt = true },
				{
					"sickill/vim-pasta",
					opt = true,
					setup = function()
						require("chad_loader").do_not_defer("vim-pasta")
						require("chad_loader").lazy_load({
							events = { "BufRead", "BufNewFile" },
							plugins = "vim-pasta",
							condition = function()
								return true
							end,
						})
					end,
				}, -- fix block paste for Neovim
				{
					"christoomey/vim-sort-motion",
					opt = true,
					setup = function()
						require("chad_loader").on_file_open("vim-sort-motion")
					end,
				}, -- gs to sort
				{
					"tpope/vim-commentary",
					requires = { "tpope/vim-repeat" },
					opt = true,
					setup = function()
						require("chad_loader").on_file_open("vim-commentary")
					end,
				}, -- gc<motion> to (un)comment
				{
					"oncomouse/vim-surround",
					requires = { "tpope/vim-repeat" },
					opt = true,
					setup = function()
						require("chad_loader").on_file_open("vim-surround")
					end,
				}, -- ys to add, cs to change, ds to delete. f, F for function, t, T for tag
				{
					"wellle/targets.vim",
					requires = { "tpope/vim-repeat" },
					opt = true,
					setup = function()
						require("chad_loader").on_file_open("targets.vim")
					end,
				}, -- add next block n]) targets, plus words in commas (a,), asterisks (a*), etc
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
					module = "project_nvim",
					setup = function()
						require("chad_loader").on_file_open("project.nvim")
					end,
				}, -- Set project root
				-- Editor Enhancements:
				{
					"oncomouse/vim-lion",
					opt = true,
					setup = function()
						require("chad_loader").on_file_open("vim-lion")
					end,
				}, -- gl and gL to align
				{
					"haya14busa/vim-asterisk",
					config = function()
						vim.keymap.set("", "*", "<Plug>(asterisk-*)")
						vim.keymap.set("", "#", "<Plug>(asterisk-#)")
						vim.keymap.set("", "g*", "<Plug>(asterisk-g*)")
						vim.keymap.set("", "g#", "<Plug>(asterisk-g#)")
						-- vim.keymap.set("", "z*", "<Plug>(asterisk-z*)")
						-- vim.keymap.set("", "gz*", "<Plug>(asterisk-gz*)")
						-- vim.keymap.set("", "z#", "<Plug>(asterisk-z#)")
						-- vim.keymap.set("", "gz#", "<Plug>(asterisk-gz#)")
						vim.g["asterisk#keeppos"] = 1
					end,
					requires = { "tpope/vim-repeat" },
					opt = true,
					setup = function()
						require("chad_loader").on_file_open("vim-asterisk")
					end,
				}, -- Fancy * and # bindings
				{
					"vim-scripts/ReplaceWithRegister",

					requires = { "tpope/vim-repeat" },
					opt = true,
					setup = function()
						require("chad_loader").on_file_open("ReplaceWithRegister")
					end,
				}, -- gr{motion} or grr or gr in visual to replace with register
				{
					"cohama/lexima.vim", -- Autopairs
					setup = function()
						vim.g.lexima_enable_endwise_rules = 0 -- Disable endwise in Lexima
						vim.g.lexima_disable_closetag = 1
					end,
					event = "InsertEnter",
					config = require("dotfiles.plugins.lexima"),
					-- Configured in ~/dotfiles/conf/vim/lua/dotfiles/plugins/lexima.lua
				},
				{
					"michaeljsmith/vim-indent-object",
					opt = true,
					setup = function()
						require("chad_loader").on_file_open("vim-indent-object")
					end,
				}, -- ii, ai, aI for indent-based textobjects
				-- Extra functionality + UI:
				{
					"kyazdani42/nvim-web-devicons",
					cond = require("dotfiles.utils.use_termguicolors"),
					module = "nvim-web-devicons",
				}, -- Icons, used in the statusline
				{
					"ibhagwan/fzf-lua",
					keys = { { "n", "<C-p>" }, { "n", "<leader>a" } },
					cmd = { "FzfLua", "Files", "Buffers", "GitStatus" },
					config = require("dotfiles.plugins.fzf-lua"),
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
					"justinmk/vim-dirvish",
					opt = true,
					setup = function()
						require("chad_loader").do_not_defer("vim-dirvish")
						require("chad_loader").on_directory("vim-dirvish")
					end,
				}, -- Directory display
				{ "roginfarrer/vim-dirvish-dovish", after = "vim-dirvish" }, -- Add useful commands to vim-dirvish
				--      Create file: a,
				--      Create directory: A,
				--      Delete under cursor: dd,
				--      Rename under cursor: r,
				--      Yank under cursor (or visual selection): yy,
				--      Copy file to current directory: pp,
				--      Move file to current directory: PP
				{
					"L3MON4D3/LuaSnip",
					config = require("dotfiles.plugins.luasnip"),
					module = "luasnip", -- Is loaded when null-ls loads luasnip completion
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
						{ "nvim-lua/plenary.nvim", module = "plenary" },
						{ "williamboman/mason.nvim", module = "mason" },
					},
					config = require("dotfiles.plugins.null-ls"),
					module = "null-ls",
					setup = function()
						require("chad_loader").on_file_open("null-ls.nvim")
					end,
					-- Configured in ~/dotfiles/conf/vim/lua/dotfiles/plugins/null-ls/init.lua
				}, -- Format and Diagnostics
				{
					"neovim/nvim-lspconfig",
					requires = {
						{ "williamboman/mason.nvim", module = "mason" },
						{ "williamboman/mason-lspconfig.nvim", module = "mason-lspconfig" },
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
					config = require("dotfiles.plugins.nvim-lspconfig"),
					-- Configured in ~/dotfiles/conf/vim/lua/dotfiles/plugins/nvim-lspconfig/init.lua
				}, -- LSP
				{
					"anuvyklack/hydra.nvim",
					config = require("dotfiles.plugins.hydra"),
					module = "hydra",
					setup = function()
						require("chad_loader").on_file_open("hydra.nvim")
					end,
					-- Configured in ~/dotfiles/conf/vim/lua/dotfiles/plugins/hydra.lua
				}, -- Repeating keys mode (used for window resizing, atm)
				{
					"chentoast/marks.nvim",
					module = "marks",
					setup = function()
						require("chad_loader").on_file_open("marks.nvim")
					end,
					config = function()
						require("marks").setup({})
					end,
				},
				{
					"nvim-treesitter/nvim-treesitter",
					config = require("dotfiles.plugins.nvim-treesitter"),
					run = function()
						vim.cmd([[TSUpdate]])
					end,
					requires = {
						{ "windwp/nvim-ts-autotag", after = "nvim-treesitter" },
						{ "RRethy/nvim-treesitter-endwise", after = "nvim-treesitter" },
						{
							"JoosepAlviste/nvim-ts-context-commentstring", -- Contextual commentstring
							after = "nvim-treesitter",
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
					module = "nvim-treesitter",
					setup = function()
						require("chad_loader").do_not_defer("nvim-treesitter")
						require("chad_loader").on_file_open("nvim-treesitter")
					end,
					-- Configured in ~/dotfiles/conf/vim/lua/dotfiles/plugins/nvim-treesitter.lua
				}, -- Treesitter-based Syntax
				-- Non-Treesitter Syntax:
				{
					"preservim/vim-markdown",
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
					"Pocco81/true-zen.nvim",
					branch = "dev",
					module = "true-zen",
					setup = function()
						require("chad_loader").on_file_open("true-zen.nvim")
					end,
					config = require("dotfiles.plugins.true-zen"),
					-- Configured in ~/dotfiles/conf/vim/lua/dotfiles/plugins/true-zen.lua
				},
				-- Appearance:
				{
					"oncomouse/lushwal.nvim",
					opt = true,
					cmd = "LushwalCompile",
					setup = function()
						vim.api.nvim_create_autocmd("ColorSchemePre", {
							group = "dotfiles-settings",
							pattern = "lushwal",
							command = "PackerLoad lushwal.nvim",
						})
					end,
					requires = { { "rktjmp/lush.nvim", opt = true }, { "rktjmp/shipwright.nvim", opt = true } },
					config = function()
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
								gina = true,
								markdown = true,
							},
						}
					end,
				}, -- Colorscheme
				{
					"oncomouse/nvim-colorizer.lua",
					module = "colorizer",
					setup = function()
						require("chad_loader").lazy_load({
							events = { "BufRead", "BufNewFile" },
							plugins = "nvim-colorizer.lua",
							condition = function()
								return true
							end,
						})
					end,
					config = function()
						if vim.opt.termguicolors:get() then
							require("colorizer").setup({
								"*", -- Load everywhere
								"!packer", -- Except packer buffers
								html = { names = true, RRGGBBAA = false },
								css = { css = true, RRGGBBAA = false },
								scss = {
									css = true,
									RRGGBBAA = false,
									custom_matcher = require("colorizer/sass").variable_matcher,
								},
							}, {
								names = false, -- Turn off highlighting color words in non-HTML/CSS settings
								RRGGBBAA = true,
								mode = "background", -- Could be background, foreground, or virtualtext
							})
							-- Attach the variable matcher to scss buffers:
							vim.api.nvim_create_autocmd("FileType", {
								group = "dotfiles-settings",
								pattern = "scss",
								callback = require("colorizer/sass").attach_to_buffer,
							})
							vim.cmd("ColorizerAttachToBuffer")
						end
					end,
				}, -- Highlight colors in files
				{
					"rebelot/heirline.nvim",
					config = require("dotfiles.plugins.heirline"),
				}, -- Statusline
				-- Configured in ~/dotfiles/conf/vim/lua/dotfiles/plugins/heirline.lua
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

-- Update Packer.nvim automatically:
vim.api.nvim_create_autocmd("BufWritePost", {
	group = "dotfiles-settings",
	pattern = "*/plugins/*.lua",
	command = "source <afile> | PackerCompile",
})
-- Install packer.nvim, if it isn't present:
if vim.fn.empty(vim.fn.glob(install_path)) == 1 then
	vim.fn.jobstart({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path }, {
		on_exit = function()
			plugins().sync()
		end,
	})
end

return plugins
