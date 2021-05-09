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
	use 'airblade/vim-rooter' -- Set project root
	use 'Konfekt/FastFold' -- Better fold support
	use 'wellle/targets.vim' -- add next block n]) targets, plus words in commas (a,), asterisks (a*), etc
	use 'cohama/lexima.vim' -- Autopairs + Endwise
	use { 'norcalli/nvim-colorizer.lua', opt = true } -- HTML codes and HTML color words to colors
	use { 'windwp/nvim-ts-autotag', opt = true }  -- Automatically close HTML tags
	-- Git Support:
	use { 'lambdalisue/gina.vim', opt = true, cmd = { 'Gina' } } -- :Gina status to schedule; :Gina commit to commit
	-- FZF Support:
	use { 'junegunn/fzf.vim', disable = (not vim.fn.executable('fzf')), opt = true, cmd = { 'Files', 'Buffers' } }  -- Add shorcuts for FZF
	-- Syntax:
	use 'nvim-treesitter/nvim-treesitter'
	use 'nvim-treesitter/nvim-treesitter-textobjects'
	use 'plasticboy/vim-markdown' -- Markdown Syntax
	use { 'godlygeek/tabular', opt = true, cmd = { 'Tabular', 'TableFormat' } } -- :Tabular /| to auto-align tables (also :TableFormat in markdown)
	use 'cakebaker/scss-syntax.vim' -- SCSS Syntax
	use 'oncomouse/vim-fish' -- Fish Syntax & Async Completion
	-- LSP:
	use 'neovim/nvim-lspconfig' -- LSP Configuration
end)
