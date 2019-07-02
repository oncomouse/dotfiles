" Git Support:
" Git Gutter {{
  let g:gitgutter_sign_added = "✚"
  let g:gitgutter_sign_modified = "…"
  let g:gitgutter_sign_removed = "✖"
  let g:gitgutter_sign_removed_first_line = "✖"
  let g:gitgutter_sign_modified_removed = "…"
"}}
" AsyncRun compatibility with vim-fugitive {{
  " command! -bang -nargs=* -complete=file Make AsyncRun -program=make @ <args>
" }}
" Gina {{
  let &previewheight = 14
  call gina#custom#command#option('status', '--opener', &previewheight . 'split')
  call gina#custom#command#option('commit', '--opener', &previewheight . 'split')
  call gina#custom#command#option('status', '--group', 'short')
  call gina#custom#command#option('commit', '--group', 'short')
" }}
