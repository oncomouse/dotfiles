function! dotfiles#autocomplete#fzf#init() abort
	command! -bang -nargs=? -complete=dir Files
		\ call fzf#vim#files(<q-args>, fzf#vim#with_preview({'options': ['--reverse', '--info=inline']}), <bang>0)
	let $FZF_DEFAULT_OPTS .= ' --reverse'
	let g:fzf_layout = { 'window': { 'width': 1, 'height': 0.4, 'yoffset': 1, 'border': 'top' } }
	" let $FZF_DEFAULT_COMMAND = 'fd --type f --hidden'
	let g:fzf_action = {
		\ 'ctrl-s': 'split',
		\ 'ctrl-v': 'vsplit',
		\ 'ctrl-t': 'tabnew',
		\ 'ctrl-e': 'edit',
		\ }
	let g:fzf_nvim_statusline = 0 " disable statusline overwriting
	" Standard dotfiles bindings:
	nmap <Plug>(dotfiles-files) <cmd>Files<CR>
	nmap <Plug>(dotfiles-home-files) <cmd>Files ~<CR>
	nmap <Plug>(dotfiles-buffers) <cmd>Buffers<CR>
	nmap <Plug>(dotfiles-windows) <cmd>Windows<CR>
	nmap <Plug>(dotfiles-lines) <cmd>BLines<CR>
	nmap <Plug>(dotfiles-commands) <cmd>Commands<CR>
endfunction
