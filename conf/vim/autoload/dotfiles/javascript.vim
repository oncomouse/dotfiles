function! dotfiles#javascript#ftplugin() abort
  " Use semistandard as a linter:
  " let g:ale_javascript_standard_executable = 'semistandard'
  " let g:ale_typescript_standard_executable = 'semistandard'
  " Use our semistandard fixer:
  " let b:ale_fixers = [function('ale#fixers#semistandard#Fix')]
  let b:ale_fixers = ['prettier']
  " Suppress eslint error:
  let g:ale_javascript_eslint_suppress_missing_config = 1

  " Set makeprg:
  let &l:makeprg = 'npx semistandard --no-install'
  set errorformat+=%f:\ line\ %l\\,\ col\ %c\\,\ %m,%-G%.%#

  if &filetype =~# 'javascript'
    set formatprg=prettier\ --parser=babel
  else
    set formatprg=prettier\ --parser=babel-ts
  endif
endfunction

function! dotfiles#javascript#after_ftplugin() abort
endfunction

