local xdg = require("dotfiles.utils.xdg")
local packer_path = xdg("XDG_DATA_HOME") .. "/packer.nvim"
local pack_path = packer_path .. "/pack"
local install_path = pack_path .. "/packer/opt/packer.nvim"
local compile_path = packer_path .. "/plugin"

vim.g.nvim_ref_devel = false

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
				{ "nvim-lua/plenary.nvim", module = { "plenary", "plenary.async", "plenary.curl" } },

				-- Editor Enhancements:

				"sickill/vim-pasta", -- fix block paste for Neovim
				"christoomey/vim-sort-motion", -- gs to sort
				"tpope/vim-sleuth", -- Automatically set indent

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
						"mini.align",
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
					},
					-- Configured in ~/dotfiles/conf/vim/after/plugin/mini-nvim.lua
				}, -- Lots of plugins. We use mini.ai for textobjects; mini.comment for commenting; mini.indentscope for indent-based textobjects (ii, ai); mini.surround for surround (ys to add, cs to change, ds to delete)

				{
					"ggandor/leap.nvim",
					config = function()
						require("leap").add_default_mappings()
						require("leap-spooky").setup({
							affixes = {},
						})
						require("flit").setup()

						-- leap-spooky working with arbitrary textobjects!
						local H = {}
						H.cache = {}

						-- Copied from leap-spooky.nvim
						function H.v_exit()
							local mode = vim.fn.mode(1)
							if mode:match("o") then
								return ""
							end
							-- v/V/<C-v> exits the corresponding Visual mode if already in it.
							return mode:sub(1, 1)
						end
						function H.get_motion_force()
							local force = ""
							local mode = vim.fn.mode(1)
							if mode:sub(2) == "oV" then
								force = "V"
							elseif mode:sub(2) == "o" then
								force = ""
							end
							return force
						end
						-- End copied form leap-spooky.nvim

						-- Copied from mini.nvim/ai
						function H.echo(msg, is_important)
							-- Construct message chunks
							msg = type(msg) == "string" and { { msg } } or msg
							table.insert(msg, 1, { "(mini.ai) ", "WarningMsg" })

							-- Avoid hit-enter-prompt
							local max_width = vim.o.columns * math.max(vim.o.cmdheight - 1, 0) + vim.v.echospace
							local chunks, tot_width = {}, 0
							for _, ch in ipairs(msg) do
								local new_ch = { vim.fn.strcharpart(ch[1], 0, max_width - tot_width), ch[2] }
								table.insert(chunks, new_ch)
								tot_width = tot_width + vim.fn.strdisplaywidth(new_ch[1])
								if tot_width >= max_width then
									break
								end
							end

							-- Echo. Force redraw to ensure that it is effective (`:h echo-redraw`)
							vim.cmd([[echo '' | redraw]])
							vim.api.nvim_echo(chunks, is_important, {})
						end
						function H.unecho()
							if H.cache.msg_shown then
								vim.cmd([[echo '' | redraw]])
							end
						end
						function H.user_textobject_id(ai_type)
							-- Get from user single character textobject identifier
							local needs_help_msg = true
							vim.defer_fn(function()
								if not needs_help_msg then
									return
								end

								local msg =
									string.format("Enter `%s` textobject identifier (single character) ", ai_type)
								H.echo(msg)
								H.cache.msg_shown = true
							end, 1000)
							local char_ok, char = pcall(vim.fn.getcharstr)
							needs_help_msg = false
							H.unecho()

							-- Terminate if couldn't get input (like with <C-c>) or it is `<Esc>`
							if not char_ok or char == "\27" then
								return nil
							end

							if char:find("^[%w%p%s]$") == nil then
								H.message("Input must be single character: alphanumeric, punctuation, or space.")
								return nil
							end

							return char
						end
						-- End copied from mini.nvim/ai

						local ai_types = {
							"a",
							"i",
						}
						local types = {
							"r",
							"m",
							"R",
							"M",
						}
						local modes = { "x", "o" }

						for _, ai_type in pairs(ai_types) do
							for _, mode in pairs(modes) do
								for _, type in pairs(types) do
									vim.keymap.set(mode, string.format("%s%s", ai_type, type), function()
										local target_windows = nil
										local keeppos = type == "r" or type == "R"
										local target_curwin = type == "r" or type == "m"
										if target_curwin then
											target_windows = { vim.fn.win_getid() }
										else
											target_windows = require("leap.util").get_enterable_windows()
										end
										local yank_paste = (
											false
											and keeppos
											and vim.v.operator == "y"
											and vim.v.register == '"'
										)
										local to = H.user_textobject_id(ai_type)
										if to == nil then
											return
										end
										require("leap").leap({
											action = require("leap-spooky").spooky_action(function()
												return H.v_exit()
													.. "v"
													.. vim.v.count1
													.. string.format("%s%s", ai_type, to)
													.. H.get_motion_force()
											end, {
												keeppos = keeppos,
												on_return = yank_paste and "p",
											}),
											target_windows = target_windows,
										})
									end)
								end
							end
						end
					end,
					requires = {
						{ "ggandor/leap-spooky.nvim" },
						{ "ggandor/flit.nvim" },
					},
				},

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
						vim.keymap.set("", "*", "<Plug>(asterisk-z*)")
						vim.keymap.set("", "#", "<Plug>(asterisk-z#)")
						vim.keymap.set("", "g*", "<Plug>(asterisk-gz*)")
						vim.keymap.set("", "g#", "<Plug>(asterisk-gz#)")
						vim.g["asterisk#keeppos"] = 1
					end,
					requires = { "tpope/vim-repeat" },
				}, -- Fancy * and # bindings

				{
					"cohama/lexima.vim", -- Autopairs
					event = "InsertEnter",
					config = function()
						require("dotfiles.plugins.lexima").setup()
						-- Autoclose mapping:
						vim.keymap.set("i", "<C-l>", "<Plug>(dotfiles-lexima-leave-til-eol)", { silent = true })
					end,
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
					cmd = { "GitStatus", "Files", "Buffers" },
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

				(vim.g.nvim_ref_devel and {
					"~/Projects/nvim-ref",
					config = function()
						require("nvim-ref").setup({
							bibfiles = {
								"~/SeaDrive/My Libraries/My Library/Documents/Academic Stuff/library-test.bib",
							},
						})
					end,
					rocks = {
						{ "lpeg-bibtex", server = "https://luarocks.org/dev" },
					},
				} or {
					"oncomouse/nvim-ref",
					config = function()
						require("nvim-ref").setup({
							bibfiles = { vim.g.bibfiles },
						})
					end,
					rocks = {
						{ "lpeg-bibtex", server = "https://luarocks.org/dev" },
					},
				}),

				{
					"L3MON4D3/LuaSnip",
					config = require("dotfiles.plugins.luasnip"),
					event = "InsertEnter",
					requires = {
						{
							"rafamadriz/friendly-snippets",
							event = "InsertEnter",
						}, -- Base Snippets
					},
					-- Configured in ~/dotfiles/conf/vim/lua/dotfiles/plugins/luasnip.lua
				}, -- Snippets

				{
					"jose-elias-alvarez/null-ls.nvim",
					-- Configured in ~/dotfiles/conf/vim/after/plugin/null-ls.lua
				}, -- Format and Diagnostics

				{
					"neovim/nvim-lspconfig",
					-- Configured in ~/dotfiles/conf/vim/plugin/nvim-lspconfig.lua
				}, -- LSP

				{
					"hrsh7th/nvim-cmp",
					module = "cmp",
					requires = {
						{ "hrsh7th/cmp-nvim-lsp", module = "cmp_nvim_lsp" },
						{ "saadparwaiz1/cmp_luasnip", after = "LuaSnip" },
						{ "hrsh7th/cmp-nvim-lua", ft = "lua" },
						{ "mtoohey31/cmp-fish", ft = "fish" },
					},
					config = require("dotfiles.plugins.cmp"),
					-- Configured in ~/dotfiles/conf/vim/lua/dotfiles/plugins/cmp.lua
				}, -- Completion (used as a super omnifunc (C-X C-O))

				"anuvyklack/hydra.nvim", -- Repeating keys mode (used for window resizing, atm)
				-- Configured in ~/dotfiles/conf/vim/plugin/hydra.lua

				"gaoDean/autolist.nvim", -- Auto-increment and indent/dedent lists
				-- Configured in ~/dotfiles/conf/vim/after/plugin/autolist-nvim.lua

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
						{ "nvim-treesitter/nvim-treesitter-textobjects", after = "nvim-treesitter" }, -- Configuration for treesitter objects
						{ "windwp/nvim-ts-autotag", after = "nvim-treesitter" },
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
					"oncomouse/vim-markdown",
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

				{
					"NvChad/nvim-colorizer.lua",
				}, -- Highlight colors in files
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
