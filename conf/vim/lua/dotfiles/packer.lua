--luacheck: globals vim use dotfiles
dotfiles = _G.dotfiles or {}
local xdg = require("dotfiles.utils.xdg")

local packer_path = xdg("XDG_DATA_HOME") .. "/packer/desktop"
vim.opt.packpath:append({ packer_path })
vim.opt.runtimepath:append({ packer_path })
-- Download Packer.nvim:
local packer_dir = packer_path .. '/pack'
-- Source: https://github.com/datwaft/nvim/blob/master/init.lua
local function ensure (user, repo, opt)
	opt = false or opt
	-- Ensures a given github.com/USER/REPO is cloned int the pack directory.
	local install_path = string.format("%s/packer/%s/%s", packer_dir, opt and "opt" or "start", repo)
	if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
		vim.fn.execute(string.format("!git clone https://github.com/%s/%s %s", user, repo, install_path))
		if opt then
			vim.fn.execute(string.format("packadd %s", repo))
		end
	end
end
ensure("wbthomason", "packer.nvim", true)

-- Load Packer.nvim:
vim.cmd("packadd packer.nvim")
return require("packer").startup({
	function()
		use { "wbthomason/packer.nvim", opt = true }
		-- Getting Started:
		use "tpope/vim-sensible" -- Agreeable vim settings:
		use "xero/securemodelines" -- Secure modelines
		use {
			"oncomouse/vim-grep",
			cmd = { "Grep", "LGrep" }
		} -- :Grep and :LGrep
		use "lambdalisue/pastefix.vim" -- fix block paste for Neovim
		use "tpope/vim-commentary" -- gc<motion> to (un)comment
		-- General Editing:
		use "sickill/vim-pasta" -- Indentation-forward pasting
		use "tpope/vim-repeat"
		use "oncomouse/vim-surround" -- ys to add, cs to change, ds to delete. f, F for function, t, T for tag
		-- Additionally supports csf and dsf for functions
		-- Using 'matchpairs' for substituting
		vim.g.rooter_patterns = {
			"Rakefile",
			 "package.json",
			 ".git/",
			 "Gemfile",
			 "pyproject.toml",
			 "setup.py",
		}
		-- Set path expansion to pwd only, especially with vim-rooter running:
		vim.opt.path=",,"
		use "airblade/vim-rooter" -- Set project root
		vim.g.fastfold_savehook = 1
		vim.g.fastfold_fold_command_suffixes =	{"x","X","a","A","o","O","c","C", "r", "R", "m", "M"}
		vim.g.fastfold_fold_movement_commands = {"]z", "[z", "zj", "zk"}
		vim.g.fastfold_minlines = 0
		use "Konfekt/FastFold" -- Better fold support
		use "wellle/targets.vim" -- add next block n]) targets, plus words in commas (a,), asterisks (a*), etc
		use {
			"cohama/lexima.vim",
			config = function()
				local function make_rule(at, e, filetype, syntax)
					return {
						char = "<CR>",
						input = "<CR>",
						input_after = "<CR>" .. e,
						at = at,
						except = "\\C\\v^(\\s*)\\S.*%#\\n%(%(\\s*|\\1\\s.+)\\n)*\\1" .. e,
						filetype = filetype,
						syntax = syntax,
					}
				end
				function dotfiles.extend_endwise_for_lua()
					-- Lua endwise rules:
					vim.fn["lexima#add_rule"](make_rule("^\\s*if\\>.*then\\%(.*[^.:@$]\\<end\\>\\)\\@!.*\\%#", "end", "lua", {}))
					vim.fn["lexima#add_rule"](make_rule("^\\s*\\%(for\\|while\\)\\>.*do\\%(.*[^.:@$]\\<end\\>\\)\\@!.*\\%#", "end", "lua", {}))
					vim.fn["lexima#add_rule"](make_rule("^\\s*\\%(local\\)\\=.*function\\>\\%(.*[^.:@$]\\<end\\>\\)\\@!.*\\%#", "end", "lua", {}))
				end
				vim.cmd[[autocmd! dotfiles-settings FileType lua lua dotfiles.extend_endwise_for_lua()]]
				vim.api.nvim_set_keymap("i", "<C-l>", "<C-r>=lexima#insmode#leave_till_eol(\"\")<CR>", { silent = true, noremap = true })
			end
		} -- Autopairs + Endwise
		use {
			"norcalli/nvim-colorizer.lua",
			config = function()
				if vim.fn.exists("+termguicolors") == 1 then
					require('colorizer').setup{
						'*',
						markdown = {
							names = false
						},
						text={
							names = false
						},
						["gina-commit"] = {
							names = false
						},
					}
				end
			end
		} -- HTML codes and HTML color words to colors
		use {
			"lambdalisue/gina.vim",
			cmd = { "Gina" },
			config = function()
				vim.fn["gina#custom#command#option"]("status", "--opener", vim.o.previewheight .. "split")
				vim.fn["gina#custom#command#option"]("commit", "--opener", vim.o.previewheight .. "split")
				vim.fn["gina#custom#command#option"]("diff", "--opener", vim.o.previewheight .. "split")
				vim.fn["gina#custom#command#option"]("status", "--group", "short")
				vim.fn["gina#custom#command#option"]("commit", "--group", "short")
				vim.fn["gina#custom#command#option"]("diff", "--group", "short")
				-- Implement vim-fugitive commands in Gina:
				vim.fn["gina#custom#mapping#nmap"]("status", "cc", ":<C-u>Gina commit<CR>", {noremap = 1, silent = 1})
			end
		} -- :Gina status to schedule; :Gina commit to commit
		-- FZF Add to RTP or Install:
		-- Macos
		if vim.fn.isdirectory("/usr/local/opt/fzf") == 1 then
			use	"/usr/local/opt/fzf"
		-- Arch
		elseif vim.fn.isdirectory("/usr/share/vim/vimfiles") == 1 then
			use "/usr/share/vim/vimfiles"
		-- Local install
		elseif vim.fn.isdirectory("~/.fzf") == 1 then
			use "~/.fzf"
		else
			use {
				"junegunn/fzf",
				run = "./install --all",
			} -- Fallback FZF install
		end
		use {
			"junegunn/fzf.vim",
			cmd = { "Files", "Buffers", "Windows", "BLines", "Commands" },
			config = function()
				vim.cmd[[command! -bang -nargs=? -complete=dir Files call fzf#vim#files(<q-args>, fzf#vim#with_preview({'options': ['--reverse', '--info=inline']}), <bang>0)]]
				vim.g.fzf_layout = { window = { width = 1, height = 0.4, yoffset = 1, border = 'top' } }
				vim.g.fzf_action = {
					["ctrl-s"] = 'split',
					["ctrl-v"] = 'vsplit',
					["ctrl-t"] = 'tabnew',
					["ctrl-e"] = 'edit',
				}
				vim.g.fzf_nvim_statusline = 0 -- disable statusline overwriting
			end
		}	-- Add shorcuts for FZF
		-- Syntax:
		use {
			"nvim-treesitter/nvim-treesitter",
			requires = {
				{ "windwp/nvim-ts-autotag", ft = { "html", "javascript", "javascriptreact", "xml" } },
				{ "nvim-treesitter/nvim-treesitter-textobjects", ft = require("dotfiles.utils.ts_filetypes").ts_types },
			},
			ft = require("dotfiles.utils.ts_filetypes").ts_types,
			run = ":TSUpdate",
			branch = "0.5-compat",
			config = function()
				require('nvim-treesitter.configs').setup{
					ensure_installed = "maintained",
					highlight = {
						enable = true,
					},
					indent = {
						enable = false,
					},
					autotag = {
						enable = true,
					},
					textobjects = {
						select = {
							enable = true,

							-- Automatically jump forward to textobj, similar to targets.vim
							lookahead = true,

							keymaps = {
								-- You can use the capture groups defined in textobjects.scm
								["af"] = "@function.outer",
								["if"] = "@function.inner",
								["ac"] = "@class.outer",
								["ic"] = "@class.inner",
							}
						}
					},
				}
				require("dotfiles.utils.ts_filetypes").ts_type_autocmds()
			end
		} -- Treesiter with highlighting, folding, textobjects, and tag-closing for HTML-like languages
		vim.g.vim_markdown_frontmatter = 1 -- Format YAML
		vim.g.vim_markdown_strikethrough = 0 -- Don"t format strikethrough
		vim.g.vim_markdown_conceal = 0 -- Don"t conceal
		vim.g.vim_markdown_conceal_code_blocks = 0 -- Don"t conceal code blocks
		vim.g.vim_markdown_math = 1 -- Do process MathJaX and LaTeX math
		use { "oncomouse/vim-fish", ft = { "fish" } } -- Fish Syntax
		use { "plasticboy/vim-markdown", ft = { "markdown" } } -- Markdown Syntax
		use {
			"godlygeek/tabular",
			cmd={ "Tabular", "TableFormat" }
		} -- :Tabular /| to auto-align tables (also :TableFormat in markdown)
		-- LSP:
		use {
			"neovim/nvim-lspconfig",
			config = function()
				require('dotfiles.nvim_lsp')
			end,
			ft = {
				"css",
				"html",
				"javascript",
				"json",
				"lua",
				"markdown",
				"python",
				"ruby",
				"scss",
				"sh",
				"vim",
				"yaml",
			}
		} -- LSP Configuration
	end,
	config = {
		package_root = packer_dir,
		compile_path = packer_path .. "/plugin/packer_compiled.vim",
	}
})
