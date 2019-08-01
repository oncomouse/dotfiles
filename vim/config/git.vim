" Git Support:
" Git Gutter {{{
  let g:gitgutter_realtime = 1
  let g:gitgutter_eager = 1
  let g:gitgutter_max_signs = 1500
  let g:gitgutter_diff_args = '-w'
  let g:gitgutter_sign_added = "✚"
  let g:gitgutter_sign_modified = "…"
  let g:gitgutter_sign_removed = "✖"
  let g:gitgutter_sign_removed_first_line = "✖"
  let g:gitgutter_sign_modified_removed = "…"
" }}}
" Gina {{{
  let &previewheight = 14
  call gina#custom#command#option('status', '--opener', &previewheight . 'split')
  call gina#custom#command#option('commit', '--opener', &previewheight . 'split')
  call gina#custom#command#option('diff', '--opener', &previewheight . 'split')
  call gina#custom#command#option('status', '--group', 'short')
  call gina#custom#command#option('commit', '--group', 'short')
  call gina#custom#command#option('diff', '--group', 'short')
  " Implement vim-fugitive commands in Gina:
  call gina#custom#mapping#nmap('status', 'cc', ':<C-u>Gina commit<CR>', {'noremap': 1, 'silent': 1})
  ca Gpush Gina push
  ca Gstatus Gina status
  ca Gdiff Gina diff
  ca Gcommit Gina commit
  augroup gina-autocmd
    autocmd FileType gina-status set nu rnu
    " Disable indentLine for status:
    autocmd FileType gina-status let b:indentLine_enabled=0 | let b:indentLine_ConcealOptionSet = 0
    autocmd FileType diff setlocal nofoldenable
  augroup END
" }}}
" # vim:foldmethod=marker

