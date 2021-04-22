" let g:dwm_map_keys = 0

nnoremap <Plug>(dwm-create) <cmd>call dwm#create()<CR>
nnoremap <Plug>(dwm-rotate-clockwise) <cmd>call dwm#rotate(1)<CR>
nnoremap <Plug>(dwm-rotate-counterclockwise) <cmd>call dwm#rotate(0)<CR>
nnoremap <Plug>(dwm-zoom) <cmd>call dwm#zoom()<CR>
nnoremap <Plug>(dwm-close) <cmd>call dwm#close()<CR>

if get(g:, 'dwm_map_keys', 1)
  nmap <silent> <m-j> <Plug>(dwm-rotate-clockwise)
  nmap <silent> <m-k> <Plug>(dwm-rotate-counterclockwise)
  nmap <silent> <m-space> <Plug>(dwm-create)
  nmap <silent> <m-,> <Plug>(dwm-zoom)
  nmap <silent> <m-c> <Plug>(dwm-close)

  if has('autocmd')
    augroup dwm
      autocmd!
      autocmd VimEnter * call dwm#auto_enter()
      autocmd WinNew * call dwm#auto_enter()
      autocmd WinClosed * exe 'call dwm#closed(' . expand('<afile>') . ')'
    augroup end
  endif
endif
