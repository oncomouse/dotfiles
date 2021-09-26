--luacheck: globals vim
local install_path = vim.fn.stdpath('data')..'/site/pack/packer/opt/packer.nvim'
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

vim.cmd [[packadd packer.nvim]]
return require('packer').startup(function(use)
	use { 'wbthomason/packer.nvim', opt = true }
	-- use { 'dstein64/vim-startuptime', cmd = 'StartupTime' }
	use 'tpope/vim-sensible'
	use 'xero/securemodelines'
	use { 'stevearc/qf_helper.nvim' , config = function()
		require("qf_helper").setup({
			quickfix = {
				default_bindings = false,
			},
			loclist = {
				default_bindings = false,
			}
		})
	end}
	use 'sickill/vim-pasta' -- fix block paste for Neovim
	use 'tpope/vim-commentary' -- gc<motion> to (un)comment
	use 'tpope/vim-repeat'
	use 'justinmk/vim-dirvish' -- File browser
	use 'oncomouse/vim-surround' -- ys to add, cs to change, ds to delete. f, F for function, t, T for tag
	vim.opt.path=",,"
	vim.g.rooter_patterns = {
		'Rakefile',
		'package.json',
		'.git/',
		'Gemfile',
		'pyproject.toml',
		'setup.py',
		'Makefile',
	}
	use 'airblade/vim-rooter' -- Set CWD for projects
	use 'wellle/targets.vim' -- add next block n]) targets, plus words in commas (a,), asterisks (a*), etc
	use 'cohama/lexima.vim' -- Autopairs + Endwise
	use { 'norcalli/nvim-colorizer.lua', event = 'BufReadPre', config = function()
		if vim.opt.termguicolors:get() then
			require("colorizer").setup({
				['*'] = {},
				markdown = { names = false },
				text = { names = false },
				["gina-commit"] = { names = false },
				css = { css = true },
				scss = { css = true },
			})
		end
	end }
	use 'junegunn/fzf.vim' -- Add shorcuts for FZF
	vim.cmd [[command! -bang -nargs=? -complete=dir Files call fzf#vim#files(<q-args>, fzf#vim#with_preview({'options': ['--reverse', '--info=inline']}), <bang>0)]]
	vim.g.fzf_layout = { window = { width = 1, height = 0.4, yoffset = 1, border = 'top' } }
	vim.g.fzf_action = {
		['ctrl-s'] = 'split',
		['ctrl-v'] = 'vsplit',
		['ctrl-t'] = 'tabnew',
		['ctrl-e'] = 'edit',
	}
	vim.g.fzf_nvim_statusline = 0 -- disable statusline overwriting
	use { 'lambdalisue/gina.vim', cmd = 'Gina', config = function()
		vim.fn['gina#custom#command#option']('status', '--opener', vim.opt.previewheight:get() .. 'split')
		vim.fn['gina#custom#command#option']('commit', '--opener', vim.opt.previewheight:get() .. 'split')
		vim.fn['gina#custom#command#option']('diff', '--opener', vim.opt.previewheight:get() .. 'split')
		vim.fn['gina#custom#command#option']('status', '--group', 'short')
		vim.fn['gina#custom#command#option']('commit', '--group', 'short')
		vim.fn['gina#custom#command#option']('diff', '--group', 'short')
		-- Implement vim-fugitive commands in Gina:
		vim.fn['gina#custom#mapping#nmap']('status', 'cc', ':<C-u>Gina commit<CR>', {noremap = 1, silent = 1})
	end }

	-- Treesitter: {{{
	local ts_types = require('dotfiles.utils.ts_filetypes').ts_types
	use { 'nvim-treesitter/nvim-treesitter-textobjects',  ft = ts_types }
	use { 'windwp/nvim-ts-autotag', ft = { 'html', 'javascript', 'javascriptreact' } }
	use { 'nvim-treesitter/nvim-treesitter', ft = ts_types, run = function()
		vim.cmd [[
			packadd nvim-treesitter
			TSUpdate
		]]
	end, config = function()
		require('nvim-treesitter.configs').setup({
			ensure_installed = 'maintained',
			highlight = { enable = true },
			indent = { enable = true },
			autotag = { enable = true },
			textobjects = { select = {
				enable = true,
				lookahead = true,
				keymaps = {
					['af'] = '@function.outer',
					['if'] = '@function.inner',
					['ac'] = '@class.outer',
					['ic'] = '@class.inner',
				}
			} }
		})
	end}
	require('dotfiles.utils.ts_filetypes').ts_type_autocmds()
	-- }}}

	-- Non-Treesitter Syntax: {{{
	use { 'plasticboy/vim-markdown', ft = 'markdown' } -- Markdown Syntax
	vim.g.vim_markdown_frontmatter = 1 -- Format YAML
	vim.g.vim_markdown_strikethrough = 0 -- Don't format strikethrough
	vim.g.vim_markdown_conceal = 0 -- Don't conceal
	vim.g.vim_markdown_conceal_code_blocks = 0 -- Don't conceal code blocks
	vim.g.vim_markdown_math = 1 -- Do process MathJaX and LaTeX math
	use { 'godlygeek/tabular',  cmd = { 'Tabularize', 'Tabular' }}
	-- }}}
	-- LSP: {{{
	local lsp_types = {
		'fish',
		'css',
		'html',
		'javascript',
		'json',
		'lua',
		'markdown',
		'python',
		'ruby',
		'scss',
		'sh',
		'vim',
		'yaml'
	}
	use {
		'neovim/nvim-lspconfig',
		requires ={
			'L3MON4D3/LuaSnip',
			'nvim-lua/plenary.nvim',
			'jose-elias-alvarez/null-ls.nvim'
		},
		ft = lsp_types,
		config = function()
			require('dotfiles.nvim_lsp')
		end
	}
-- }}}
end)
-- }}}
-- # vim:foldmethod=marker
