" Adapted from nvim-orgmode and ported back to vimscript (it's easier).
" This opens and closes folds using the <Tab> key
function! s:cycle() abort
	if foldlevel('.') > 0
		exe 'silent! norm!z' . (foldclosed('.') >= 0 ? 'o' : 'a')
	endif
endfunction

nnoremap <Tab> <cmd>call <SID>cycle()<CR>
