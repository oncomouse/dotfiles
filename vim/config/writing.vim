" Writing:
" Pandoc shortcuts {{{
augroup pandoc-shortcuts
  autocmd!
  autocmd FileType pandoc,markdown nmap <C-b> ysiw*lysiw*
augroup END
" }}}
" deoplete-biblatex {{{
let g:deoplete#sources#biblatex#bibfile = g:bibliography_file
let g:deoplete#sources#biblatex#addinfo = v:true
call deoplete#custom#source('biblatex', 'filetypes', ['markdown'])
" }}}
" Deoplete ignore sources{{{
call deoplete#custom#option('ignore_sources', {'markdown': ['around', 'buffer', 'file', 'tmux']})
" }}}
" Vim-markdown {{{
let g:vim_markdown_frontmatter = 1
let g:vim_markdown_strikethrough = 0
let g:vim_markdown_conceal = 0
let g:vim_markdown_conceal_code_blocks = 0
let g:vim_markdown_math = 1
function! ToggleConcealLevel()
  if &conceallevel == 0
    setlocal conceallevel=2
  else
    setlocal conceallevel=0
  endif
endfunction
" <leader>cc turns conceal on and off
augroup markdown_higlight
  autocmd!
  autocmd FileType markdown,pandoc nnoremap <silent> <leader>cc :call ToggleConcealLevel()<CR>
augroup END
" }}}
" Limelight {{{
  let g:limelight_conceal_ctermfg='black'
"}}}
" Goyo {{{
  " function! s:goyo_enter()
  "   " Trigger Limelight
  "   Limelight
  "   " For some reason, lightline-bufferline causes lightline to reenable, so
  "   " we have to turn it off on these events:
  "   augroup lightline_goyo
  "     autocmd!
  "     autocmd BufWritePost,TextChanged,TextChangedI * call lightline#disable()
  "   augroup END
  "   ALEToggle
  "   silent !tmux set status off
  "   silent !tmux list-panes -F '\#F' | grep -q Z | tmux resize-pane -Z
  "   set noshowmode
  "   set noshowcmd
  "   set scrolloff=999
  " endfunction

  " function! s:goyo_leave()
  "   Limelight!
  "   augroup lightline_goyo
  "     autocmd!
  "   augroup END
  "   ALEToggle
  "   silent !tmux set status on
  "   silent !tmux list-panes -F '\#F' | grep -q Z && tmux resize-pane -Z
  "   set showmode
  "   set showcmd
  "   set scrolloff=0
  " endfunction

"}}}
" Easy Align {{{
  " Start interactive EasyAlign in visual mode (e.g. vipga)
  xmap ga <Plug>(EasyAlign)
  " Start interactive EasyAlign for a motion/text object (e.g. gaip)
  nmap ga <Plug>(EasyAlign)
"}}}
" Pencil {{{
  " VimForWriters recommended Pencil config:
  let g:pencil#wrapModeDefault = 'soft'
  let g:pencil#textwidth = 74
  let g:pencil#joinspaces = 0 " I *think* this is what's fixing the weird problem we were having
  let g:pencil#cursorwrap = 1
  let g:pencil#conceallevel = 3
  let g:pencil#concealcursor = 'c'
  let g:pencil#softDetectSample = 20
  let g:pencil#softDetectThreshold = 130
  " source: http://www.naperwrimo.org/wiki/index.php?title=Vim_for_Writers
"}}}
" Writing Environment: {{{
  function! s:limelight(on) abort
    if a:on == 1
      if exists(':Limelight')
        Limelight
      endif
    else
      if exists(':Limelight')
        Limelight!
      endif
    endif
  endfunction
  augroup writing
    autocmd!
    autocmd FileType pandoc,markdown call lexical#init()
                                   \ | call litecorrect#init()
                                   \ | call textobj#sentence#init()
                                   \ | call pencil#init()
    " Make sure j and k work with word wrap turned on:
    " autocmd FileType pandoc,markdown nmap j gj
    " autocmd FileType pandoc,markdown nmap k gk
    " Ensure that lightline doesn't freak out when we use Goyo:
    autocmd! User GoyoEnter call <SID>goyo_enter()
    autocmd! User GoyoLeave call <SID>goyo_leave()
    " Enable Limelight in pandoc and turn it off when we aren't in pandoc:
    autocmd BufEnter * if &filetype=='markdown'|call <SID>limelight(1)|end
    autocmd BufLeave * if &filetype=='markdown'|call <SID>limelight(0)|end
  augroup END
" }}}
" # vim:foldmethod=marker
