let s:pack_loaded = get(g:, 'dotfiles_loaded_pack', 0)
let g:enable_todo = 1
" Highlight a block and type "@" to run a macro on the block:
xnoremap <silent> @ :<C-u>call visualat#execute_macro_over_visual_range()<CR>

" Grep project:
function s:grep_or_qfgrep()
	if &buftype ==# 'quickfix'
		let l:input = input('QFGrep/')
		if len(l:input) > 0
			let l:prefix = getwininfo(win_getid())[0].loclist ? 'L' : 'C'
			execute l:prefix . 'filter /'.l:input.'/'
		endif
	else
		let l:input = input('Grep/')
		if len(l:input) > 0
			execute (s:pack_loaded ? 'G' : 'silent! g') . 'rep ' . l:input
		endif
	endif 
endfunction
nnoremap <silent> <leader>/ <cmd>call <SID>grep_or_qfgrep()<CR>

" Calculator (not sure how this works):
inoremap <C-A> <C-O>yiW<End>=<C-R>=<C-R>0<CR>

" Shortcut to view current syntax highlighting group:
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
	\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
	\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" List Bindings: {{{
nnoremap <silent> <leader>d :call dotfiles#lists#toggle('Location List', 'l')<CR>
nnoremap <silent> <leader>q :call dotfiles#lists#toggle('Quickfix List', 'c')<CR>
"}}}
" Uniform Visual Motion Toggle: {{{
map <leader>w <cmd>call edit_mode#toggle()<CR>
" }}}
" FZF Bindings: {{{
if s:pack_loaded
	nmap <silent> <c-p> <cmd>Files<CR>
	nmap <silent> <leader>a <cmd>Buffers<CR>
endif
" }}}
