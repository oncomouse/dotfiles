let s:loaded = []
let s:running = 0
let s:autocmd = {}

augroup dynapac
	autocmd!
augroup END

function! s:to_a(v) abort
	return type(a:v) == type([]) ? a:v : [a:v]
endfunction

" Remove all autocmds associated with this plugin:
function! s:remove_autocmds(plug) abort
	if has_key(s:autocmd, a:plug)
		for cmd in s:autocmd[a:plug]
			execute 'delcommand ' . cmd
		endfor
	endif
endfunction

function! s:lod_cmd(cmd, bang, l1, l2, args, plug) abort
	call dynapac#load(a:plug)
	execute printf('%s%s%s %s', (a:l1 == a:l2 ? '' : (a:l1.','.a:l2)), a:cmd, a:bang, a:args)
endfunction

function! dynapac#load(plug) abort
	if index(s:loaded, a:plug) < 0
		call s:remove_autocmds(a:plug)
		let l:pack = split(a:plug, '/')[-1]
		execute 'packadd ' . l:pack 
		execute 'doautocmd User ' . l:pack
		call insert(s:loaded, a:plug)
	endif
endfunction

function! dynapac#delay(plug, opts) abort
	if s:running
		call minpac#add(a:plug, extend(a:opts, { 'type': 'opt' }))
	endif
	if has_key(a:opts, 'cmd')
		let s:autocmd[a:plug] = s:to_a(a:opts.cmd)
		for cmd in s:autocmd[a:plug]
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
	if s:running
		let l:opts = get(a:, 2, {})
		call minpac#add(a:1, extend(l:opts, { 'type': 'opt' }))
	else
		execute 'packadd ' . split(a:1, '/')[-1]
	endif
endfunction

function! dynapac#init(...) abort
	let s:running = get(a:000, 0, 0)
	let l:opts = get(a:000, 1, {})
	let l:path = get(l:opts, 'dir', split(&packpath, ',')[0])
	let l:opts['dir'] = l:path
	" Download Minpac:
	if s:running
		if empty(glob(l:path.'/pack/minpac/opt/minpac'))
			if executable('git')
				silent execute '!git clone --depth 1 https://github.com/k-takata/minpac "'.l:path.'/pack/minpac/opt/minpac"'
			endif
		endif

		" Load Minpac:
		packadd minpac
		if exists('g:loaded_minpac')
			call minpac#init(l:opts)
			call minpac#add('k-takata/minpac', { 'type': 'opt' })
		else
			echoerr "Could not load minpac. Perhaps your Internet is not working or you don't have git?"
		endif
	endif
endfunction
