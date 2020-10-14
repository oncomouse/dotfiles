function! dotfiles#javascript#ftplugin() abort
  " let b:ale_linters = ['eslint', 'standard']
  if &filetype =~# 'typescript'
    let b:ale_disable_lsp = 0
  endif
  " Use our semistandard fixer:
  let b:ale_fixers = [function('ale#fixers#semistandard#Fix')]

  " Set makeprg:
  let &l:makeprg = 'npx semistandard --no-install'
  set errorformat+=%f:\ line\ %l\\,\ col\ %c\\,\ %m,%-G%.%#

  " Setup mucomplete omnifunc:
  setlocal omnifunc=TSOmniFunc
endfunction

function! dotfiles#javascript#after_ftplugin() abort
  " Remap LSP commands for javascript:
  nmap <buffer><silent> <F2> :TSRename<CR>
  nmap <buffer><silent> gd   :TSDef<CR>
  nmap <buffer><silent> gy   :TSTypeDef<CR>
  nmap <buffer><silent> gr   :TSRefs<CR>
  nmap <buffer><silent> K    :TSDoc<CR>
endfunction

