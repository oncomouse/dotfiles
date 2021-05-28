--luacheck: globals vim use
local function packpath()
	return (os.getenv("XDG_DATA_HOME") and os.getenv("XDG_DATA_HOME") or os.getenv("HOME")..'/.local/share') .. '/packer/desktop'
end

local pp = packpath()
vim.o.packpath = vim.o.packpath .. "," .. pp
vim.o.runtimepath = vim.o.runtimepath .. "," .. pp
-- Download Packer.nvim:
local packer_dir = pp .. '/pack'
local packer_compile_dir = pp .. '/plugin'
if vim.fn.isdirectory(packer_dir) == 0 then
	vim.fn.system('git clone --depth 1 https://github.com/wbthomason/packer.nvim "'..packer_dir..'/packer/opt/packer.nvim"')
end

-- Load Minpac:
vim.cmd("packadd packer.nvim")
return require("packer").startup({
	function()
		use { 'wbthomason/packer.nvim', opt = true }
		-- Getting Started:
		use "tpope/vim-sensible" -- Agreeable vim settings:
		use "xero/securemodelines" -- Secure modelines
		use { "oncomouse/vim-grep", cmd = { "Grep", "LGrep" } } -- :Grep and :LGrep
		use "tpope/vim-commentary" -- gc<motion> to (un)comment
		-- General Editing:
		use "sickill/vim-pasta" -- Indentation-forward pasting
		use "tpope/vim-repeat"
		use "oncomouse/vim-surround" -- ys to add, cs to change, ds to delete. f, F for function, t, T for tag
		use { "airblade/vim-rooter",  setup = function()
			vim.g.rooter_patterns = {
				"Rakefile",
				 "package.json",
				 ".git/",
				 "Gemfile",
				 "pyproject.toml",
				 "setup.py",
			}
			-- Set path expansion to pwd only, especially with vim-rooter running:
			vim.o.path=",,"
		end} -- Set project root
		use { "Konfekt/FastFold", setup = function()
			vim.g.fastfold_savehook = 1
			vim.g.fastfold_fold_command_suffixes =	{"x","X","a","A","o","O","c","C", "r", "R", "m", "M"}
			vim.g.fastfold_fold_movement_commands = {"]z", "[z", "zj", "zk"}
			vim.g.fastfold_minlines = 0
		end } -- Better fold support
		use "wellle/targets.vim" -- add next block n]) targets, plus words in commas (a,), asterisks (a*), etc
		use { "cohama/lexima.vim",
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
				local function extend_endwise()
					-- Lua endwise rules:
					vim.fn["lexima#add_rule"](make_rule("^\\s*if\\>.*then\\%(.*[^.:@$]\\<end\\>\\)\\@!.*\\%#", "end", "lua", {}))
					vim.fn["lexima#add_rule"](make_rule("^\\s*\\%(for\\|while\\)\\>.*do\\%(.*[^.:@$]\\<end\\>\\)\\@!.*\\%#", "end", "lua", {}))
					vim.fn["lexima#add_rule"](make_rule("^\\s*\\%(local\\)\\=.*function\\>\\%(.*[^.:@$]\\<end\\>\\)\\@!.*\\%#", "end", "lua", {}))
				end
				extend_endwise()
				-- inoremap <C-l> <C-r>=lexima#insmode#leave_till_eol("")<CR>
			end
		} -- Autopairs + Endwise
		use { "norcalli/nvim-colorizer.lua", opt = true } -- HTML codes and HTML color words to colors
		-- Git Support:
		use { "lambdalisue/gina.vim", config = function()
			vim.fn["gina#custom#command#option"]("status", "--opener", vim.o.previewheight .. "split")
			vim.fn["gina#custom#command#option"]("commit", "--opener", vim.o.previewheight .. "split")
			vim.fn["gina#custom#command#option"]("diff", "--opener", vim.o.previewheight .. "split")
			vim.fn["gina#custom#command#option"]("status", "--group", "short")
			vim.fn["gina#custom#command#option"]("commit", "--group", "short")
			vim.fn["gina#custom#command#option"]("diff", "--group", "short")
			-- Implement vim-fugitive commands in Gina:
			vim.fn["gina#custom#mapping#nmap"]("status", "cc", ":<C-u>Gina commit<CR>", {noremap = 1, silent = 1})
		end} -- :Gina status to schedule; :Gina commit to commit
		-- FZF Support:
		use { "/usr/local/opt/fzf", cond = function() return vim.fn.isdirectory("/usr/local/opt/fzf") == 1 end }
		-- Arch
		use { "/usr/share/vim/vimfiles", cond = function() return vim.fn.isdirectory("/usr/share/vim/vimfiles") == 1 end }
		-- Local install
		use { "~/.fzf", cond = function() return vim.fn.isdirectory("~/.fzf") == 1 end }
		use { "junegunn/fzf.vim", cond = function() return vim.fn.executable("fzf") == 1 end, config = function()
			vim.cmd[[command! -bang -nargs=? -complete=dir Files call fzf#vim#files(<q-args>, fzf#vim#with_preview({'options': ['--reverse', '--info=inline']}), <bang>0)]]
			vim.g.fzf_layout = { window = { width = 1, height = 0.4, yoffset = 1, border = 'top' } }
			vim.g.fzf_action = {
				["ctrl-s"] = 'split',
				["ctrl-v"] = 'vsplit',
				["ctrl-t"] = 'tabnew',
				["ctrl-e"] = 'edit',
			}
			vim.g.fzf_nvim_statusline = 0 -- disable statusline overwriting
			-- Standard dotfiles bindings:
			vim.api.nvim_set_keymap("n", "<Plug>(dotfiles-files)", "<cmd>Files<CR>", {})
			vim.api.nvim_set_keymap("n", "<Plug>(dotfiles-home-files)", "<cmd>Files ~<CR>", {})
			vim.api.nvim_set_keymap("n", "<Plug>(dotfiles-buffers)", "<cmd>Buffers<CR>", {})
			vim.api.nvim_set_keymap("n", "<Plug>(dotfiles-windows)", "<cmd>Windows<CR>", {})
			vim.api.nvim_set_keymap("n", "<Plug>(dotfiles-lines)", "<cmd>BLines<CR>", {})
			vim.api.nvim_set_keymap("n", "<Plug>(dotfiles-commands)", "<cmd>Commands<CR>", {})
		end}  -- Add shorcuts for FZF
		-- Syntax:
		use { "nvim-treesitter/nvim-treesitter",
			run = ":TSUpdate",
			config = function()
				require('dotfiles.treesitter')
			end
		}
		use { "windwp/nvim-ts-autotag", ft={"html", "javascript", "javascriptreact"} }  -- Automatically close HTML tags
		use { "nvim-treesitter/nvim-treesitter-textobjects" }
		use { "plasticboy/vim-markdown", ft={ "markdown" }, setup = function()
			vim.g.vim_markdown_frontmatter = 1 -- Format YAML
			vim.g.vim_markdown_strikethrough = 0 -- Don"t format strikethrough
			vim.g.vim_markdown_conceal = 0 -- Don"t conceal
			vim.g.vim_markdown_conceal_code_blocks = 0 -- Don"t conceal code blocks
			vim.g.vim_markdown_math = 1 -- Do process MathJaX and LaTeX math
		end } -- Markdown Syntax
		use { "godlygeek/tabular", cmd={ "Tabular", "TableFormat" } } -- :Tabular /| to auto-align tables (also :TableFormat in markdown)
		-- LSP:
		use { "neovim/nvim-lspconfig", config = function()
			require('dotfiles.nvim_lsp')
		end } -- LSP Configuration
	end,
	config = {
		package_root = packer_dir,
		compile_path = packer_compile_dir .. "/packer_compiled.vim",
	}
})
