" Git Support:
scriptencoding utf-8
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
" }}}
" # vim:foldmethod=marker

