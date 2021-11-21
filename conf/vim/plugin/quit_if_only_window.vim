" https://stackoverflow.com/a/39307414
function! s:NextNormalWindow() abort
	for i in range(1, winnr('$'))
		let buf = winbufnr(i)

		" skip unlisted buffers
		if !buflisted(buf)
			continue
		endif

		" skip temporary buffers with buftype set
		if getbufvar(buf, '&buftype') !=# ''
			continue
		endif

		" skip the preview window
		if getwinvar(i, '&previewwindow')
			continue
		endif

		" skip current window
		if i == winnr()
			continue
		endif

		return i
	endfor

	return -1
endfunction

function! s:QuitIfOnlyWindow() abort
	let l:buftype = getbufvar(winbufnr(winnr()), '&buftype')
	if l:buftype !=# 'quickfix' && l:buftype !=# 'help'
		return
	endif

	" Check if there is more than one window
	if s:NextNormalWindow() == -1
		" Check if there is more than one tab page
		if tabpagenr('$') == 1
			" Before quitting Vim, delete the special buffer so that
			" the '0 mark is correctly set to the previous buffer.
			" Also disable autocmd on this command to avoid unnecessary
			" autocmd nesting.
			if winnr('$') == 1
				if has('autocmd')
					noautocmd bdelete
				endif
			endif
			quit
		else
			" Note: workaround for the fact that in new tab the buftype is set
			" too late (and sticks during this WinEntry autocmd to the old -
			" potentially quickfix/help buftype - that would automatically
			" close the new tab and open the buffer in copen window instead
			" New tabpage has previous window set to 0
			if tabpagewinnr(tabpagenr(), '#') != 0
				let l:last_window = 0
				if winnr('$') == 1
					let l:last_window = 1
				endif
				close
				if l:last_window == 1
					" Note: workaround for the same bug, but w.r.t. Airline
					" plugin (it needs to refresh buftype and status line after
					" last special window autocmd close on a tab page
					if exists(':AirlineRefresh')
						execute 'AirlineRefresh'
					endif
				endif
			endif
		endif
	endif
endfunction

" autoclose last open location/quickfix/help windows on a tab
if has('autocmd')
	aug AutoCloseAllQF
		au!
		autocmd WinEnter * nested call s:QuitIfOnlyWindow()
	aug END
endif

