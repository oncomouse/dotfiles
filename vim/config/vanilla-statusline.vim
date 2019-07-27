" Statusline:
" Lightline Git Status {{{
let g:lightline#gitdiff#indicator_added = '✚'
let g:lightline#gitdiff#indicator_deleted = '✖'
let g:lightline#gitdiff#indicator_modified = '…'
let g:lightline#gitdiff#separator = ' ' 
" }}}
" 
" status bar colors
function! ModeCurrent() abort
    let l:modecurrent = mode()
    " use get() -> fails safely, since ^V doesn't seem to register
    " 3rd arg is used when return of mode() == 0, which is case with ^V
    " thus, ^V fails -> returns 0 -> replaced with 'V Block'
    let l:modelist = toupper(get(g:currentmode, l:modecurrent, 'V·Block '))
    let l:current_status_mode = l:modelist
    return l:current_status_mode
endfunction
" function! ModeSetHighlighting(mode)
"   " Insert mode: blue
"   echo a:mode
"   if a:mode=~#"^[vV\<C-v>]"
"     highlight StatusLine ctermbg=9 ctermfg=13 cterm=bold
"     highlight CursorLineNR ctermfg=9 ctermbg=10
"   elseif a:mode == "i"
"     highlight StatusLine ctermbg=2 ctermfg=13 cterm=bold
"     highlight CursorLineNR ctermfg=2 ctermbg=10
"   " Replace mode: red
"   elseif a:mode=~#"^[rR]"
"     highlight StatusLine ctermbg=1 ctermfg=13 cterm=bold
"     highlight CursorLineNR ctermfg=9 ctermbg=10
"   else
"     highlight StatusLine ctermbg=4 ctermfg=13 cterm=bold
"     highlight CursorLineNr ctermfg=8 ctermbg=10
"   endif
"   return ""
" endfunction
" augroup color-mode-switches
"   autocmd!
  " autocmd CursorMoved,WinEnter,BufWinEnter,FileType,SessionLoadPost * call ModeSetHighlighting(v:insertmode)
  " autocmd InsertLeave * call ModeSetHighlighting("n")
  " autocmd BufEnter * call ModeSetHighlighting(v:insertmode)
  " au InsertEnter * hi statusline ctermfg=0 ctermbg=5
  " au InsertLeave * hi statusline ctermfg=0 ctermbg=6
" augroup END
function! ModeSetHighlighting(mode)
  " Insert mode: blue
  " highlight! link StatusLine NONE
  if a:mode=~#"^[vV\<C-v>]"
    highlight! link StatusLine StatusLine_Visual
  elseif a:mode == "i"
    highlight! link StatusLine StatusLine_Insert
  elseif a:mode=~#"^[rR]"
    highlight! link StatusLine StatusLine_Replace
  elseif a:mode == "t"
    highlight! link StatusLine StatusLine_Terminal
  endif
  highlight! link StatusLine StatusLine_Normal
  return a:mode . " "
endfunction
" autocmd InsertEnter * call ModeSetHighlighting(v:insertmode)
" Status line
" default: set statusline=%f\ %h%w%m%r\ %=%(%l,%c%V\ %=\ %P%)

" Status Line Custom
let g:currentmode={
    \ 'n'  : 'Normal',
    \ 'no' : 'Normal·Operator Pending',
    \ 'v'  : 'Visual',
    \ 'V'  : 'V·Line',
    \ '^V' : 'V·Block',
    \ 's'  : 'Select',
    \ 'S'  : 'S·Line',
    \ '^S' : 'S·Block',
    \ 'i'  : 'Insert',
    \ 'R'  : 'Replace',
    \ 'Rv' : 'V·Replace',
    \ 'c'  : 'Command',
    \ 'cv' : 'Vim Ex',
    \ 'ce' : 'Ex',
    \ 'r'  : 'Prompt',
    \ 'rm' : 'More',
    \ 'r?' : 'Confirm',
    \ '!'  : 'Shell',
    \ 't'  : 'Terminal'
    \}
function! HiGroup() abort
  return execute("hi StatusLine")
endfunction
set laststatus=2
set noshowmode
set statusline=
set statusline+=%{ModeSetHighlighting(mode())}
set statusline+=%0*\ %n\                                 " Buffer number
set statusline+=%1*%{HiGroup()}
" set statusline+=%1*\ %<%F%m%r%h%w\                       " File path, modified, readonly, helpfile, preview
" set statusline+=%3*│                                     " Separator
" set statusline+=%2*\ %Y\                                 " FileType
" set statusline+=%3*│                                     " Separator
" set statusline+=%2*\ %{''.(&fenc!=''?&fenc:&enc).''}     " Encoding
" set statusline+=\ (%{&ff})                               " FileFormat (dos/unix..)
 set statusline+=%=                                       " Right Side
" set statusline+=%2*\ col:\ %02v\                         " Colomn number
" set statusline+=%3*│                                     " Separator
" set statusline+=%1*\ ln:\ %02l/%L\ (%3p%%)\              " Line number / total lines, percentage of document
set statusline+=%0*\ %{ModeCurrent()}\  " The current mode

hi User1 ctermfg=7 ctermbg=10
hi User2 ctermfg=7 ctermbg=8
hi User3 ctermfg=8 ctermbg=8
hi User4 ctermfg=10 ctermbg=10
" # vim:foldmethod=marker
