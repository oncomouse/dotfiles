" Writing:
" Autocomplete {{{
if g:complete_package =~# 'coc.nvim'
  call dotfiles#autocomplete#coc_nvim#writing()
else
  call dotfiles#autocomplete#deoplete#writing()
endif
"
" }}}
" Pandoc shortcuts {{{
augroup pandoc-shortcuts
  autocmd!
  autocmd FileType pandoc,markdown nmap <C-b> ysiw*lysiw*
  autocmd FileType pandoc,markdown call dotfiles#autocomplete#deoplete#writing()
augroup END
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
" # vim:foldmethod=marker
