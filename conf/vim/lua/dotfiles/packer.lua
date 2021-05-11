-- luacheck: globals use
function packpath()
	return os.getenv("XDG_DATA_HOME") and os.getenv("XDG_DATA_HOME") or os.getenv("HOME.'/.local/share')") .. '/packer'
end

-- Download Paq:
local packer_dir = packpath() .. '/pack/packer'
if vim.fn.isdirectory(packer_dir) == 0 then
	vim.fn.system('git clone https://github.com/wbthomason/packer.nvim "'..packer_dir..'/start/packer.nvim"')
end
-- vim.cmd("packadd packer.nvim")

return require('packer').startup(function()
	use 'wbthomason/packer.nvim'
	-- Getting Started:
	use 'tpope/vim-sensible' -- Agreeable vim settings:
	use 'xero/securemodelines' -- Secure modelines
	use { 'oncomouse/vim-grep', cmd={ 'Grep', 'LGrep' } } -- :Grep and :LGrep
	use 'tpope/vim-commentary' -- gc<motion> to (un)comment
	-- General Editing:
	use 'sickill/vim-pasta' -- Indentation-forward pasting
	use 'tpope/vim-repeat'
	use 'oncomouse/vim-surround' -- ys to add, cs to change, ds to delete. f, F for function, t, T for tag
	use { 'airblade/vim-rooter', config = function()
		vim.g.rooter_patterns = {
			'Rakefile',
			'package.json',
			'.git/',
			'Gemfile',
			'pyproject.toml',
			'setup.py',
		}
		-- Set path expansion to pwd only, especially with vim-rooter running:
		vim.o.path=",,"
	end } -- Set project root
	use 'Konfekt/FastFold' -- Better fold support
	use 'wellle/targets.vim' -- add next block n]) targets, plus words in commas (a,), asterisks (a*), etc
	use { 'cohama/lexima.vim' , config = function()
		local function make_rule(at, ed, filetype, syntax)
			return {
				char = '<CR>',
				input = '<CR>',
				input_after = '<CR>' .. ed,
				at = at,
				except = "\\C\v^(\\s*)\\S.*%#\n%(%(\\s*|\1\\s.+)\n)*\1" .. ed,
				filetype = filetype,
				syntax = syntax,
			}
		end
		function extend_endwise()
			-- Lua endwise rules:
			vim.fn["lexima#add_rules"](make_rule('^\s*if\>.*then\%(.*[^.:@$]\<end\>\)\@!.*\%#', 'end', 'lua', {}))
			vim.fn["lexima#add_rules"](make_rule('^\s*\%(for\|while\)\>.*do\%(.*[^.:@$]\<end\>\)\@!.*\%#', 'end', 'lua', {}))
			vim.fn["lexima#add_rules"](make_rule('^\s*\%(local\)\=.*function\>\%(.*[^.:@$]\<end\>\)\@!.*\%#', 'end', 'lua', {}))
		end
		vim.cmd[[autocmd plug-settings VimEnter * call s:extend_endwise() ]]
		-- inoremap <C-l> <C-r>=lexima#insmode#leave_till_eol("")<CR>
		" }}}
	end} -- Autopairs + Endwise
	use { 'norcalli/nvim-colorizer.lua', opt = true } -- HTML codes and HTML color words to colors
	use { 'windwp/nvim-ts-autotag', opt = true }  -- Automatically close HTML tags
	-- Git Support:
	use { 'lambdalisue/gina.vim', opt = true, cmd = { 'Gina' } } -- :Gina status to schedule; :Gina commit to commit
	-- FZF Support:
	use {
		'junegunn/fzf.vim',
		disable = (not vim.fn.executable('fzf')),
		opt = true,
		cmd = {
			'Files',
			'Buffers'
		},
		config = function()
			vim.fn["dotfiles#fzf#init"]()
		end
	}  -- Add shorcuts for FZF
	-- Syntax:
	use { 'nvim-treesitter/nvim-treesitter', config = function()
		require('dotfiles.treesitter')
	end }
	use { 'nvim-treesitter/nvim-treesitter-textobjects', after = 'nvim-treesitter' }
	use 'plasticboy/vim-markdown' -- Markdown Syntax
	use { 'godlygeek/tabular', opt = true, cmd = { 'Tabular', 'TableFormat' } } -- :Tabular /| to auto-align tables (also :TableFormat in markdown)
	use 'cakebaker/scss-syntax.vim' -- SCSS Syntax
	use 'oncomouse/vim-fish' -- Fish Syntax & Async Completion
	-- LSP:
	use { 'neovim/nvim-lspconfig', opt=true, ft = {
		'css',
		'html',
		'javascript',
		'json',
		'lua',
		'markdown',
		'python',
		'ruby',
		'sh',
		'vim',
	}, config = function()
		require('dotfiles.nvim_lsp')
	end } -- LSP Configuration
end)
