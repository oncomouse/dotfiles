function! s:TmuxOrTmateExecutable()
  return (match($TMUX, 'tmate') != -1 ? 'tmate' : 'tmux')
endfunction

function! s:TmuxSocket()
  " The socket path is the first value in the comma-separated list of $TMUX.
  return split($TMUX, ',')[0]
endfunction

function! s:TmuxCommand(args)
  let cmd = s:TmuxOrTmateExecutable() . ' -S ' . s:TmuxSocket() . ' ' . a:args
  let l:x=&shellcmdflag
  let &shellcmdflag='-c'
  let retval=system(cmd)
  let &shellcmdflag=l:x
  return retval
endfunction

function s:rotate(clockwise)
  if s:TmuxCommand("display-message -p '#{window_panes}'") == 1
    call dwm#rotate(a:clockwise)
    return
  endif
  if a:clockwise == 1 && winnr() == winnr('$')
    call s:TmuxCommand('nextpane')
    return
  elseif a:clockwise == 0 && winnr() == 1
    call s:TmuxCommand('prevpane')
    return
  endif
  call dwm#rotate(a:clockwise)
endfunction

if empty($TMUX)
  nmap <silent> <m-j> <Plug>(dwm-rotate-clockwise)
  nmap <silent> <m-k> <Plug>(dwm-rotate-counterclockwise)
else
  nmap <silent> <m-j> <cmd>call <SID>rotate(1)<CR>
  nmap <silent> <m-k> <cmd>call <SID>rotate(0)<CR>
  " Leader + dwm.tmux binding calls the command in tmux:
  nmap <silent> <leader><m-space> <cmd>call <SID>TmuxCommand('newpane')<CR>
  nmap <silent> <leader><m-j> <cmd>call <SID>TmuxCommand('nextpane')<CR>
  nmap <silent> <leader><m-k> <cmd>call <SID>TmuxCommand('prevpane')<CR>
endif
nmap <silent> <m-space> <Plug>(dwm-create)
nmap <silent> <m-,> <Plug>(dwm-zoom)
nmap <silent> <m-c> <Plug>(dwm-close)
