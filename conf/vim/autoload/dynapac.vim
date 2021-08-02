" Set to true if we are installing (which means we actually call minpac):
let s:running = 0
let s:loaded = []
let s:cmds = {}
let s:events = {}
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

" This function is from vim-plug:
function! s:lod_cmd(cmd, bang, l1, l2, args, plug) abort
	call s:load(a:plug)
	execute printf('%s%s%s %s', (a:l1 == a:l2 ? '' : (a:l1.','.a:l2)), a:cmd, a:bang, a:args)
endfunction

function! s:ev(ev) abort
	for f in s:events[a:ev]
		call s:load(f)
	endfor
	execute 'autocmd! dynapac ' . a:ev
	execute 'doautocmd ' . a:ev
endfunction

function! s:load(plug) abort
	if index(s:loaded, a:plug) < 0
		call insert(s:loaded, a:plug)
		call s:remove_cmds(a:plug)
		let l:pack = split(a:plug, '/')[-1]
		execute 'packadd ' . l:pack 
		execute 'doautocmd User ' . l:pack
	endif
endfunction

function! s:add_event(ev, plug) abort
	if stridx(a:ev, ' ') < 0
		let a:ev = a:ev . ' *'
	endif
	if has_key(s:events, a:ev)
		call insert(s:events[a:ev], a:plug)
	else
		let s:events[a:ev] = [a:plug]
		execute 'autocmd! dynapac ' . a:ev . ' call s:ev("' . a:ev . '")'
	endif
endfunction

function! dynapac#add(...) abort
	let l:plug = get(a:, 1, '')
	let l:opts = get(a:, 2, {})
	if has_key(l:opts, 'cmd') || has_key(l:opts, 'ft')
		call extend(l:opts, { 'type': 'opt' })
	endif
	if s:running
		call minpac#add(l:plug, l:opts)
	else
		if has_key(l:opts, 'cmd')
			let s:cmds[l:plug] = s:to_a(l:opts.cmd)
			for cmd in s:cmds[l:plug]
				execute printf(
					\ 'command! -nargs=* -range -bang -complete=file %s call s:lod_cmd(%s, "<bang>", <line1>, <line2>, <q-args>, %s)',
					\ cmd, string(cmd), string(l:plug))
			endfor
		endif
		if has_key(l:opts, 'ft')
			for ev in s:to_a(l:opts.ft)
				call s:add_event('FileType ' . ev, l:plug)
			endfor
		endif
		if has_key(l:opts, 'event')
			for ev in s:to_a(l:opts.event)
				call s:add_event(ev, l:plug)
			endfor
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
