function! dotfiles#javascript#ftplugin() abort
  let b:ale_javascript_standard_executable = 'semistandard'
  let b:ale_fixers = ['prettier_standard']
  let b:ale_javascript_prettier_standard_executable = 'prettier-semi'
  let b:ale_javascript_prettier_standard_options = &filetype =~# '-l silent ' . ('javascript' ? '--parser=babel' : '--parser=babel-ts')
  " Suppress eslint error:
  let g:ale_javascript_eslint_suppress_missing_config = 1

  " Set makeprg:
  let &l:makeprg = 'npx semistandard --no-install'
  set errorformat+=%f:\ line\ %l\\,\ col\ %c\\,\ %m,%-G%.%#

  if &filetype =~# 'javascript'
    set formatprg=prettier-semi\ --stdin\ -l\ silent\ --parser=babel
  else
    set formatprg=prettier-semi\ --stdin\ -l\ silent\ --parser=babel-ts
  endif
endfunction

function! dotfiles#javascript#after_ftplugin() abort
endfunction

