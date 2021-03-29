function! dotfiles#javascript#ftplugin() abort
  let b:ale_fixers = ['prettier']
  " Suppress eslint error:
  let g:ale_javascript_eslint_suppress_missing_config = 1

  " Set makeprg:
  let &l:makeprg = 'npx semistandard --no-install'
  set errorformat+=%f:\ line\ %l\\,\ col\ %c\\,\ %m,%-G%.%#

  if &filetype =~# 'javascript'
    set formatprg=prettier-semi\ --stdin\ -l\ silent
  else
    set formatprg=prettier\ --parser=babel-ts
  endif
endfunction

function! dotfiles#javascript#after_ftplugin() abort
endfunction

