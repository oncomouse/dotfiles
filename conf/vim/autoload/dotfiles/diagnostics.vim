let s:ichecking = get(g:, 'dotfiles#diagnostics#indicator_checking', '…')
let s:iinfos = get(g:, 'dotfiles#diagnostics#indicator_infos', 'I:')
let s:iwarnings = get(g:, 'dotfiles#diagnostics#indicator_warnings', 'W:')
let s:ierrors = get(g:, 'dotfiles#diagnostics#indicator_errors', 'E:')
let s:iok = get(g:, 'dotfiles#diagnostics#indicator_ok', 'OK')

function! dotfiles#diagnostics#errors() abort
	if has('nvim-0.5')
		let l:count = v:lua.vim.lsp.diagnostic.get_count(0, "Error")
		return l:count == 0 ? '' : printf(s:ierrors . '%d', l:count)
	else
		return dotfiles#ale#errors()
	endif
endfunction

function! dotfiles#diagnostics#warnings() abort
	if has('nvim-0.5')
		let l:count = v:lua.vim.lsp.diagnostic.get_count(0, "Warning")
		return l:count == 0 ? '' : printf(s:iwarnings . '%d', l:count)
	else
		return dotfiles#ale#warnings()
	endif
endfunction

function! dotfiles#diagnostics#ok() abort
	if has('nvim-0.5')
		let l:ecount = v:lua.vim.lsp.diagnostic.get_count(0, "Error")
		let l:wcount = v:lua.vim.lsp.diagnostic.get_count(0, "Warning")
		return l:ecount + l:wcount == 0 ? s:iok : ''
	else
		return dotfiles#ale#ok()
	endif
endfunction

function! dotfiles#diagnostics#checking() abort
	if has('nvim-0.5')
		return ''
	else
		return dotfiles#ale#checking()
	endif
endfunction

