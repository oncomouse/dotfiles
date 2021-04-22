" Attach bottom:

let s:window_count = 1

function! dwm#create() abort
  if winnr('$') == 1
    vsplit
  else
    exe ':'.winnr('$').'windo split'
  endif
  let s:window_count += 1
endfunction

function! dwm#rotate(clockwise) abort
  exe ':wincmd ' . (a:clockwise == 1 ? 'w' : 'W') 
endfunction

function! dwm#stack(clockwise) abort
  1wincmd w
  if a:clockwise
    wincmd K
  else
    wincmd J
  endif
endfunction

function! dwm#zoom() abort
  if winnr('$') == 1
    return
  endif

  if winnr() == 1
    wincmd w
  endif

  let l:curwin = winnr()
  call dwm#stack(1)
  exec l:curwin . 'wincmd w'
  wincmd H
  call dwm#resize_master()

endfunction

function! dwm#resize_master() abort
  wincmd =
endfunction

function! dwm#close() abort
  if winnr() == 1
    close
    1wincmd H
    call dwm#resize_master()
  else
    close
  endif
  let s:window_count -= 1
endfunction

function! dwm#auto_leave() abort
  if winnr('$') < 2
    return
  endif

  " To prevent vim crashes filetype is checked
  " before any actual layout changes. So layout
  " is updated only for regular buffers. Probably
  " other buffer properties should be also checked.
  if winnr() == 1
    wincmd K
    wincmd x
    wincmd H
    call dwm#resize_master()
  endif
endfunction

function! dwm#check_layout() abort
  if &l:buflisted && !exists('w:created')
    let w:created = 1
    call dwm#auto_enter()
  endif
  if s:window_count > winnr('$')
    1wincmd H
    let s:window_count = winnr('$')
  endif
endfunction

function! dwm#auto_enter() abort
  if winnr('$') == 1
    return
  endif

  " Skip buffers without filetype
  if !len(&l:filetype)
    return
  endif

  " Skip quickfix buffers
  if &l:buftype ==# 'quickfix'
    return
  endif

  " Move new window to stack top
  wincmd K

  " Focus new window (twice :)
  call dwm#zoom()
  call dwm#zoom()
endfunction

" This code is derived from https://github.com/spolu/dwm.vim:
"==============================================================================
"    Copyright: Copyright (C) 2012 Stanislas Polu an other Contributors
"               Permission is hereby granted to use and distribute this code,
"               with or without modifications, provided that this copyright
"               notice is copied with it. Like anything else that's free,
"               dwm.vim is provided *as is* and comes with no warranty of
"               any kind, either expressed or implied. In no event will the
"               copyright holder be liable for any damages resulting from
"               the use of this software.
"==============================================================================
