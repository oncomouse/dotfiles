" Writing:
" FZF BibTeX Configuration {{{
  let $FZF_BIBTEX_CACHEDIR = '/var/tmp'
  let $FZF_BIBTEX_SOURCES = g:bibliography_file

  augroup fzf-bibtex
    autocmd!
    " Bind <ctrl+c> to citation look-up using FZF:
    autocmd FileType pandoc,text,markdown nnoremap <silent> <C-C> :call dotfiles#fzf#bibtex_run_ls('dotfiles#fzf#bibtex_cite_sink')<CR>
    autocmd FileType pandoc,text,markdown inoremap <silent> <C-C> <c-g>u<c-o>:call dotfiles#fzf#bibtex_run_ls('dotfiles#fzf#bibtex_cite_sink_insert')<CR>
  augroup END
" }}}
" CoC Source Ignore {{{
  augroup coc-pandoc
    autocmd!
    autocmd FileType pandoc call coc#config('coc.source.buffer.enable', 0)
    autocmd FileType pandoc call coc#config('coc.source.around.enable', 0)
    autocmd FileType pandoc call coc#config('coc.source.snippets.enable', 0)
    autocmd FileType pandoc call coc#config('coc.source.file.enable', 0)
  augroup END
  function! CocBufferOn() abort
    call coc#config('coc.source.buffer.enable', 1)
    call coc#config('coc.source.around.enable', 1)
  endfunction
  function! CocBufferOff() abort
    call coc#config('coc.source.buffer.enable', 0)
    call coc#config('coc.source.around.enable', 0)
  endfunction
  command CocBufferOn call CocBufferOn()
  command CocBufferOff call CocBufferOff()
" }}}
" Pandoc {{{
  " Uncomment to use the omni-func for bibliography completion:
  let g:pandoc#biblio#bibs=[g:bibliography_file]
  " Turn off folding and vim-pandoc's BibTeX support
  " let g:pandoc#modules#disabled = ['folding'] " , 'bibliography']
  " Turn off conceal
  let g:pandoc#syntax#conceal#use = 0
"}}}
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
    autocmd BufEnter * if &filetype=='pandoc'|call <SID>limelight(1)|end
    autocmd BufLeave * if &filetype=='pandoc'|call <SID>limelight(0)|end
  augroup END
" }}}
" # vim:foldmethod=marker
