" Set to true if we are installing (which means we actually call minpac):
let s:running = 0
let s:packs = []
" Despite the above data structure, it's actually easier to have a collection
" of events ordered by type:
let s:events = {}
let s:list_type = type([])
let s:func_type = type(function('has'))

augroup dynapac
	autocmd!
augroup END

" Convert to List:
function! s:to_a(v) abort
	return type(a:v) == s:list_type ? a:v : [a:v]
endfunction

" Remove all autocmds associated with this plugin:
function! s:remove_cmds(plug) abort
	for cmd in get(s:packs[a:plug][1], 'cmd', [])
		execute 'delcommand ' . cmd
	endfor
endfunction

" This function is from vim-plug:
function! s:lod_cmd(cmd, bang, l1, l2, args, plug) abort
	call s:load(a:plug)
	execute printf('%s%s%s %s', (a:l1 == a:l2 ? '' : (a:l1.','.a:l2)), a:cmd, a:bang, a:args)
endfunction

" Handle events with plugin loads attached (can also be used for function):
function! s:ev(ev) abort
	for F in s:events[a:ev]
		if type(F) == s:func_type
			call function(F)()
		else
			call s:load(F)
		endif
	endfor
	execute 'autocmd! dynapac ' . a:ev
endfunction

" Trigger VimEnter events for all start packs:
function! s:load_start() abort
	for plug in filter(copy(s:packs), 'get(v:val[1], "type", "start") == "start"')
		call s:load(plug[0], 1)
	endfor
endfunction

" Trigger various things for packs at load time:
function! s:load(plug, ...) abort
	let l:start = get(a:, 1, 0)
	if get(s:packs[a:plug][1], 'loaded', 0)
		let s:packs[a:plug][1]['loaded'] = 1
		let l:pack = split(a:plug, '/')[-1]
		if !l:start
			call s:remove_cmds(a:plug)
			execute 'packadd ' . l:pack 
		endif
		execute 'doautocmd User ' . l:pack
	endif
endfunction

" Attach an event for dynapac loading, can also be passed a function instead
" of a plug name:
function! s:add_event(ev, plug) abort
	let l:ev = stridx(a:ev, ' ') < 0 ? a:ev . ' *' : a:ev
	if has_key(s:events, l:ev)
		call insert(s:events[l:ev], a:plug)
	else
		let s:events[l:ev] = [a:plug]
		execute 'autocmd! dynapac ' . l:ev . ' call s:ev("' . l:ev . '")'
	endif
endfunction

" Add a plugin.
" Options can include:
"  - cmd: a list of commands to trigger plugin loading
"  - ft: a list of filetypes to trigger plugin loading
"  - event: a list of events to trigger plugin loading
"  - Any other options that can be passed to minpac
function! dynapac#add(plug, ...) abort
	let l:opts = get(a:, 1, {})
	if has_key(l:opts, 'cmd') || has_key(l:opts, 'ft') || has_key(l:opts, 'event')
		call extend(l:opts, { 'type': 'opt' })
	endif
	if !s:running
		call insert(s:packs, [a:plug, l:opts])
		if has_key(l:opts, 'cmd')
			for cmd in s:to_a(l:opts.cmd)
				execute printf(
					\ 'command! -nargs=* -range -bang -complete=file %s call s:lod_cmd(%s, "<bang>", <line1>, <line2>, <q-args>, %s)',
					\ cmd, string(cmd), string(a:plug))
			endfor
		endif
		if has_key(l:opts, 'ft')
			for ev in s:to_a(l:opts.ft)
				call s:add_event('FileType ' . ev, a:plug)
			endfor
		endif
		if has_key(l:opts, 'event')
			for ev in s:to_a(l:opts.event)
				call s:add_event(ev, a:plug)
			endfor
		endif
	endif
endfunction

" Start the process of loading plugins. Set the first option to 1 when we are
" actually installing plugins (when PackUpdate or PackClean are being run).
" This will also automatically install minpac if it is not found.
function! dynapac#init(...) abort
	let s:running = get(a:000, 0, 0)
	let l:opts = get(a:000, 1, {})
	let l:path = get(l:opts, 'dir', split(&packpath, ',')[0])
	let l:opts['dir'] = l:path
	" Load / Download Minpac:
	if s:running
		packadd minpac
		if exists('g:loaded_minpac')
			call minpac#init(l:opts)
			for pack in s:packs
				call minpac#add(pack[0], pack[1])
			endfor
		else
			if executable('git')
				silent execute '!git clone --depth 1 https://github.com/k-takata/minpac "'.l:path.'/pack/minpac/opt/minpac"'
				call function('dynapac#init', a:000)()
			else
				echoerr "Could not load minpac. Perhaps your Internet is not working or you don't have git?"
			endif
		endif
	else
		call s:add_event('VimEnter', function('s:load_start'))
	endif
endfunction
