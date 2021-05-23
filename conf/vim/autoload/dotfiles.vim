" https://github.com/Gavinok/dotneovim/blob/main/autoload/dotvim.vim
function! dotfiles#titlecase(type, ...) abort
  let g:type = a:type
  let g:it =  a:0
  let g:dem = a:000
  let WORD_PATTERN = '\<\(\k\)\(\k*''*\k*\)\>'
  " let UPCASE_REPLACEMENT = '\u\1\L\2'
  let UPCASE_REPLACEMENT = '\u\1\2' " Capitalize only (will preserve acroynms)

  let regbak = @@
  try
    if a:0  " Invoked from Visual mode, use '< and '> marks.
      " Back up unnamed register to avoid clobbering its contents
      if a:type ==# ''
        silent exe 'normal! `<' . a:type . '`>y'
        let titlecased = substitute(@@, WORD_PATTERN, UPCASE_REPLACEMENT, 'g')
        call setreg('@', titlecased, 'b')
        silent execute 'normal! ' . a:type . '`>p'
      else
        silent exe 'normal! `<' . a:type . '`>y'
        let @i = substitute(@@, WORD_PATTERN, UPCASE_REPLACEMENT, 'g')
        silent execute 'normal! ' . a:type . '`>"ip'
      endif
    elseif a:type ==# 'line'
      execute '''[,'']s/'.WORD_PATTERN.'/'.UPCASE_REPLACEMENT.'/ge'
    else
      silent exe 'normal! `[v`]y'
      let titlecased = substitute(@@, WORD_PATTERN, UPCASE_REPLACEMENT, 'g')
      silent exe 'normal! v`]c' . titlecased
    endif
  finally
    " Restore unnamed register to its original state
    let @@ = regbak
  endtry
endfunction
