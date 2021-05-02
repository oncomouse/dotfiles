" Initialize Autocomplete:
let g:dotfiles_mode = get(g:, 'dotfiles_mode', '')
let g:complete_package = get(g:, 'complete_package', '')

function! dotfiles#autocomplete#init() abort
	" List Management:
	call dotfiles#autocomplete#{substitute(g:complete_package, '[.-]', '_', 'g')}#init()
	" If not using Coc.nvim in desktop mode, load an LSP client and a Linter:
	if g:dotfiles_mode ==# 'desktop' && g:complete_package !=# 'coc.nvim'
		" Language Server Client:
		if has('nvim-0.5')
			call dotfiles#autocomplete#nvim_lsp#init()
		else
			call dotfiles#autocomplete#LanguageClient#init()
			" Linter:
			call dotfiles#autocomplete#ale#init()
		endif
	endif
endfunction
