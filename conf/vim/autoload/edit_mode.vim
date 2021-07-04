let g:edit_mode = 0
function! edit_mode#toggle() abort
	if g:edit_mode
		nunmap j
		nunmap k
		vunmap j
		vunmap k
		iunmap <Down>
		iunmap <Up>
		echom 'Edit Mode Off'
	else
		nnoremap <silent> j gj
		nnoremap <silent> k gk
		vnoremap <silent> j gj
		vnoremap <silent> k gk
		inoremap <silent> <Down> <C-O>gj
		inoremap <silent> <Up> <C-O>gk
		echom 'Edit Mode On'
	endif
	let g:edit_mode = !g:edit_mode
endfunction
