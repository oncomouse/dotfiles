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
function! dotfiles#lexima#extend_endwise() abort
	" Lua endwise rules:
	call lexima#add_rule(s:make_rule('^\s*if\>.*then\%(.*[^.:@$]\<end\>\)\@!.*\%#', 'end', 'lua', []))
	call lexima#add_rule(s:make_rule('^\s*\%(for\|while\)\>.*do\%(.*[^.:@$]\<end\>\)\@!.*\%#', 'end', 'lua', []))
	call lexima#add_rule(s:make_rule('^\s*\%(local\)\=.*function\>\%(.*[^.:@$]\<end\>\)\@!.*\%#', 'end', 'lua', []))
endfunction

