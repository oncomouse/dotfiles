" Adapted from nvim-orgmode and ported back to vimscript (it's easier).
" This opens and closes folds using the <Tab> key
function! s:cycle() abort
	if foldlevel('.') > 0
		if foldclosed('.') != -1
			silent! norm!zo
		else
			silent! norm!za
		end
	endif
endfunction

nnoremap <Tab> <cmd>call <SID>cycle()<CR>
