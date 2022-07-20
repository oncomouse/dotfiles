local xdg = require("dotfiles.utils.xdg")
local packer_path = xdg("XDG_DATA_HOME") .. "/packer.nvim"
local pack_path = packer_path .. "/pack"
local install_path = pack_path .. "/packer/opt/packer.nvim"
local compile_path = packer_path .. "/plugin"

local function plugins()
	local util = require("packer.util")

	return require("packer").startup({
		function(use)
			use({
				{ "wbthomason/packer.nvim", opt = true },
				"sickill/vim-pasta", -- fix block paste for Neovim
				{
					"christoomey/vim-sort-motion",
				}, -- gs to sort
				{ "tpope/vim-commentary", requires = { "tpope/vim-repeat" } }, -- gc<motion> to (un)comment
				{
					"oncomouse/vim-surround",
					requires = { "tpope/vim-repeat" },
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

					requires = { "tpope/vim-repeat" },
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
					-- Configured in ~/dotfiles/conf/vim/after/plugin/hydra.lua
				}, -- Repeating keys mode (used for window resizing, atm)
				{
					"nvim-treesitter/nvim-treesitter",
					-- Configured in ~/dotfiles/conf/vim/after/plugin/nvim-treesitter.lua
					run = function()
						vim.cmd([[TSUpdate]])
					end,
					requires = {
						"windwp/nvim-ts-autotag",
						"RRethy/nvim-treesitter-endwise",
					},
				}, -- Treesitter-based Syntax
				{
					"JoosepAlviste/nvim-ts-context-commentstring", -- Contextual commentstring
					ft = {
						"css",
						"html",
						"javascript",
						"javascriptreact",
						"lua",
						"scss",
						"typescript",
						"typescriptreact",
						"vim",
					},
				},
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
				-- Appearance:
				{
					"oncomouse/lushwal.nvim",
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
						end
						-- Attach the variable matcher to scss buffers:
						vim.api.nvim_create_autocmd("FileType", {
							group = "dotfiles-settings",
							pattern = "scss",
							callback = require("colorizer/sass").attach_to_buffer,
						})
					end,
				}, -- Highlight colors in files
				{ "rebelot/heirline.nvim" }, -- Statusline
				-- Configured in ~/dotfiles/conf/vim/plugin/heirline.lua
			})
		end,
		config = {
			package_root = pack_path,
			compile_path = util.join_paths(compile_path, "packer_compiled.lua"),
		},
	})
end

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
		vim.cmd("packadd packer.nvim")
		plugins()[cmd]()
	end, {})
end

vim.opt.runtimepath:append(packer_path)
vim.opt.packpath:append(packer_path)

-- Update Packer.nvim automatically:
vim.api.nvim_create_autocmd("BufWritePost", {
	group = "dotfiles-settings",
	pattern = "plugins/*.lua",
	command = "source <afile> | PackerCompile",
})
-- Install packer.nvim, if it isn't present:
if vim.fn.empty(vim.fn.glob(install_path)) == 1 then
	vim.fn.jobstart({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path }, {
		on_exit = function()
			vim.cmd("packadd packer.nvim")
			plugins().sync()
		end,
	})
end
