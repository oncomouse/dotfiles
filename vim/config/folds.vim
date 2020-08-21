" Folds {{{
  " Set fold method to syntax by default:
  set foldmethod=syntax
  set foldlevel=99
  " FastFold:
  nmap zuz <Plug>(FastFoldUpdate)
  let g:fastfold_savehook = 1
  let g:fastfold_fold_command_suffixes =  ['x','X','a','A','o','O','c','C', 'r', 'R', 'm', 'M']
  let g:fastfold_fold_movement_commands = [']z', '[z', 'zj', 'zk']
  let g:fastfold_minlines = 0
  augroup custom-folds
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker foldlevel=0
    autocmd FileType css setlocal foldmethod=manual
    autocmd FileType diff setlocal nofoldenable
  augroup END
" }}}
