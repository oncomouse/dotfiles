call plug#begin('~/.vim/plugged')
Plug 'tpope/vim-sensible'
Plug 'xero/securemodelines'
Plug 'oncomouse/vim-grep', { on: ['Grep', 'LGrep'] }
Plug 'stevearc/qf_helper.nvim'
function s:qf_helper()
	call luaeval('require("qf_helper").setup(_A)', {
		\ 'quickfix': {
		\		'default_bindings': v:false,
		\ },
		\ 'loclist' : {
		\		'default_bindings': v:false,
		\ }
		\ })
endfunction
autocmd dotfiles-settings VimEnter * call s:qf_helper()
Plug 'lambdalisue/pastefix.vim' " fix block paste for Neovim
Plug 'tpope/vim-commentary' " gc<motion> to (un)comment
" General Editing:
" use "sickill/vim-pasta" " Indentation-forward pasting
Plug 'tpope/vim-repeat'
Plug 'oncomouse/vim-surround' " ys to add, cs to change, ds to delete. f, F for function, t, T for tag
" Additionally supports csf and dsf for functions
" Using 'matchpairs' for substituting
let g:rooter_patterns = [
		\'Rakefile',
		\'package.json',
		\'.git/',
		\'Gemfile',
		\'pyproject.toml',
		\'setup.py',
		\]
" Set path expansion to pwd only, especially with vim-rooter running:
set path=,,
Plug 'airblade/vim-rooter' " Set project root
Plug 'wellle/targets.vim' " add next block n]) targets, plus words in commas (a,), asterisks (a*), etc
Plug 'cohama/lexima.vim' " Autopairs + Endwise
function! s:make_rule(at, end, filetype, syntax)
	return {
	\ 'char': '<CR>',
	\ 'input': '<CR>',
	\ 'input_after': '<CR>' . a:end,
	\ 'at': a:at,
	\ 'except': '\C\v^(\s*)\S.*%#\n%(%(\s*|\1\s.+)\n)*\1' . a:end,
	\ 'filetype': a:filetype,
	\ 'syntax': a:syntax,
	\ }
endfunction
function! s:extend_endwise() abort
	" Lua endwise rules:
	call lexima#add_rule(s:make_rule('^\s*if\>.*then\%(.*[^.:@$]\<end\>\)\@!.*\%#', 'end', 'lua', []))
	call lexima#add_rule(s:make_rule('^\s*\%(for\|while\)\>.*do\%(.*[^.:@$]\<end\>\)\@!.*\%#', 'end', 'lua', []))
	call lexima#add_rule(s:make_rule('^\s*\%(local\)\=.*function\>\%(.*[^.:@$]\<end\>\)\@!.*\%#', 'end', 'lua', []))
endfunction
autocmd! dotfiles-settings FileType lua s:extend_endwise()
inoremap <silent> <C-l> <C-r>=lexima#insmode#leave_till_eol("")<CR>
Plug 'norcalli/nvim-colorizer.lua'
lua <<EOF
function _G.nvim_colorizer()
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
EOF
autocmd dotfiles-settings VimEnter * call v:lua.nvim_colorizer()
Plug 'lambdalisue/gina.vim', { 'on': 'Gina' }
function! s:gina() abort
	call gina#custom#command#option('status', '--opener', vim.o.previewheight .. 'split')
	call gina#custom#command#option('commit', '--opener', vim.o.previewheight .. 'split')
	call gina#custom#command#option('diff', '--opener', vim.o.previewheight .. 'split')
	call gina#custom#command#option('status', '--group', 'short')
	call gina#custom#command#option('commit', '--group', 'short')
	call gina#custom#command#option('diff', '--group', 'short')
	" Implement vim-fugitive commands in Gina:
	call gina#custom#mapping#nmap('status', 'cc', ':<C-u>Gina commit<CR>', {noremap = 1, silent = 1})
endfunction
autocmd dotfiles-settings User gina.vim call s:gina()
Plug 'junegunn/fzf.vim', { on = ['Files', 'Buffers', 'Windows', 'Blines', 'Commands'] } " Add shorcuts for FZF
command! -bang -nargs=? -complete=dir Files call fzf#vim#files(<q-args>, fzf#vim#with_preview({'options': ['--reverse', '--info=inline'}), <bang>0)
let g:fzf_layout = { 'window': { 'width': 1, 'height': 0.4, 'yoffset': 1, 'border': 'top' } }
let g:fzf_action = {
\'ctrl-s': 'split',
\'ctrl-v': 'vsplit',
\'ctrl-t': 'tabnew',
\'ctrl-e': 'edit',
\}
let g:fzf_nvim_statusline = 0 " disable statusline overwriting
" Treesitter:
Plug 'windwp/nvim-ts-autotag', { 'for': [ 'html', 'javascript', 'javascriptreact' ] }
Plug 'nvim-treesitter/nvim-treesitter-textobjects', { 'for': luaeval('require("dotfiles.utils.ts_filetypes").ts_types') }
Plug 'nvim-treesitter/nvim-treesitter',  { 'for': luaeval('require("dotfiles.utils.ts_filetypes").ts_types'), do: { -> TSUpdate } }
lua<<EOF
	function nvim_treesitter()
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
EOF
autocmd dotfiles-settings User nvim-treesiter call v:lua.nvim_treesitter()
" Syntax:
let g:vim_markdown_frontmatter = 1 " Format YAML
let g:vim_markdown_strikethrough = 0 " Don"t format strikethrough
let g:vim_markdown_conceal = 0 " Don"t conceal
let g:vim_markdown_conceal_code_blocks = 0 " Don"t conceal code blocks
let g:vim_markdown_math = 1 " Do process MathJaX and LaTeX math
Plug 'oncomouse/vim-fish', { 'for': 'fish' } " Fish Syntax
Plug 'plasticboy/vim-markdown', { 'for': 'markdown' } " Markdown Syntax
Plug 'godlygeek/tabular', { 'on': 'Tabular', 'TableFormat' }
" LSP:
Plug 'neovim/nvim-lspconfig', { 'on': [ 'css', 'html', 'javascript', 'json', 'lua', 'markdown', 'python', 'ruby', 'scss', 'sh', 'vim', 'yaml' ] }
autocmd dotfiles-settings User nvim-lspconfig lua require('dotfiles.nvim_lsp')
call plug#end()
