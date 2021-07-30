let s:loaded = []
let s:dynapac_running = 0

augroup dynapac
	autocmd!
augroup END

function! s:to_a(v) abort
	return type(a:v) == type([]) ? a:v : [a:v]
endfunction

function! s:lod_cmd(cmd, bang, l1, l2, args, plug) abort
	execute 'delcommand ' . a:cmd
	call dynapac#load(a:plug)
	execute printf('%s%s%s %s', (a:l1 == a:l2 ? '' : (a:l1.','.a:l2)), a:cmd, a:bang, a:args)
endfunction

function! dynapac#load(plug) abort
	let l:pack = split(a:plug, "/")[-1]
	if index(s:loaded, a:plug) < 0
		execute 'packadd ' . l:pack 
		execute 'doautocmd User ' . l:pack
		call insert(s:loaded, a:plug)
	endif
endfunction

function! dynapac#delay(plug, opts) abort
	if s:dynapac_running
		call minpac#add(a:plug, extend(a:opts, { 'type': 'opt' }))
	endif
	if has_key(a:opts, 'cmd')
		for cmd in s:to_a(a:opts.cmd)
			execute printf(
				\ 'command! -nargs=* -range -bang -complete=file %s call s:lod_cmd(%s, "<bang>", <line1>, <line2>, <q-args>, %s)',
				\ cmd, string(cmd), string(a:plug))
		endfor
	elseif has_key(a:opts, 'ft')
		for ft in s:to_a(a:opts.ft)
			execute 'autocmd! dynapac FileType ' . ft . ' call dynapac#load("' . a:plug . '")'
		endfor
	endif
endfunction

function! dynapac#add(...) abort
	if s:dynapac_running
		call minpac#add(a:1, get(a:, 2, {}))
	endif
endfunction

function! dynapac#init(r, path) abort
	let s:dynapac_running = a:r
	" Download Minpac:
	if s:dynapac_running
		let l:minpac_dir = a:path
		if empty(glob(l:minpac_dir.'/pack/minpac/opt/minpac'))
			if executable('git')
				silent execute '!git clone --depth 1 https://github.com/k-takata/minpac "'.l:minpac_dir.'/pack/minpac/opt/minpac"'
			endif
		endif

		" Load Minpac:
		packadd minpac
		call minpac#init({'dir': l:minpac_dir})
	endif
endfunction
