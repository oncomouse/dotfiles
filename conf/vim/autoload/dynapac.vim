let s:loaded = []
let s:running = 0
let s:cmds = {}
let s:fts = {}
let s:list_type = type([])

augroup dynapac
	autocmd!
augroup END

" Convert to List:
function! s:to_a(v) abort
	return type(a:v) == s:list_type ? a:v : [a:v]
endfunction

" Remove all autocmds associated with this plugin:
function! s:remove_cmds(plug) abort
	if has_key(s:cmds, a:plug)
		for cmd in s:cmds[a:plug]
			execute 'delcommand ' . cmd
		endfor
	endif
endfunction

" Catch any FileType groups triggered by a dynamically loaded plugin:
function! s:trigger_ft(ft) abort
	if a:ft !=# ''
		execute 'doautocmd FileType ' . a:ft
	endif
endfunction

function! s:lod_cmd(cmd, bang, l1, l2, args, plug) abort
	call dynapac#load(a:plug)
	execute printf('%s%s%s %s', (a:l1 == a:l2 ? '' : (a:l1.','.a:l2)), a:cmd, a:bang, a:args)
endfunction

function! dynapac#load(...) abort
	let l:plug = get(a:000, 0, '')
	let l:ft = get(a:000, 1, '')
	if index(s:loaded, l:plug) < 0
		call insert(s:loaded, l:plug)
		call s:remove_cmds(l:plug)
		let l:pack = split(l:plug, '/')[-1]
		execute 'packadd ' . l:pack 
		execute 'doautocmd User ' . l:pack
		call s:trigger_ft(l:ft)
	endif
endfunction

function! dynapac#add(...) abort
	let l:plug = get(a:, 1, '')
	let l:opts = get(a:, 2, {})
	if s:running
		call minpac#add(l:plug, extend(l:opts, { 'type': 'opt' }))
	else
		if has_key(l:opts, 'cmd')
			let s:cmds[l:plug] = s:to_a(l:opts.cmd)
			for cmd in s:cmds[l:plug]
				execute printf(
					\ 'command! -nargs=* -range -bang -complete=file %s call s:lod_cmd(%s, "<bang>", <line1>, <line2>, <q-args>, %s)',
					\ cmd, string(cmd), string(l:plug))
			endfor
		elseif has_key(l:opts, 'ft')
			let s:fts[l:plug] = s:to_a(l:opts.ft)
			for ft in s:fts[l:plug]
				execute 'autocmd! dynapac FileType ' . ft . ' call dynapac#load("' . l:plug . '", "' . ft . '")'
			endfor
		elseif get(l:opts, 'type', '') !=# 'opt'
			execute 'packadd ' . split(l:plug, '/')[-1]
		endif
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
		else
			echoerr "Could not load minpac. Perhaps your Internet is not working or you don't have git?"
		endif
	endif
endfunction
