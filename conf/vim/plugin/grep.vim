if has_key(g:, 'dotfiles_mode')
  if has('nvim')
    command! -nargs=+ -complete=file_in_path -bar Grep  call grep#grep(<f-args>)
    command! -nargs=+ -complete=file_in_path -bar LGrep call grep#lgrep(<f-args>)
  else
    command! -nargs=+ -complete=file_in_path -bar Grep  cgetexpr grep#grep(<f-args>)
    command! -nargs=+ -complete=file_in_path -bar LGrep lgetexpr grep#lgrep(<f-args>)
  endif

  cnoreabbrev <expr> grep  (getcmdtype() ==# ':' && getcmdline() ==# 'grep')  ? 'Grep'  : 'grep'
  cnoreabbrev <expr> lgrep (getcmdtype() ==# ':' && getcmdline() ==# 'lgrep') ? 'LGrep' : 'lgrep'

  augroup quickfix
    autocmd!
    autocmd QuickFixCmdPost [^l]* call grep#open_list(1)
    autocmd QuickFixCmdPost l* call grep#open_list(0)
  augroup END
endif
