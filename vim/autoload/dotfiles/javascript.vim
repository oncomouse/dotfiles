function! dotfiles#javascript#ftplugin() abort
  " Enable tsserver:
  " let b:ale_disable_lsp = 0
  " Use semistandard as a linter:
  let g:ale_javascript_standard_executable = 'semistandard'
  let g:ale_typescript_standard_executable = 'semistandard'
  " Use our semistandard fixer:
  let b:ale_fixers = [function('ale#fixers#semistandard#Fix')]
  " Suppress eslint error:
  let g:ale_javascript_eslint_suppress_missing_config = 1

  " Set makeprg:
  let &l:makeprg = 'npx semistandard --no-install'
  set errorformat+=%f:\ line\ %l\\,\ col\ %c\\,\ %m,%-G%.%#

  " Setup omnifunc for muComplete:
  " setlocal omnifunc=ale#completion#OmniFunc
endfunction

function! dotfiles#javascript#after_ftplugin() abort
  " Remap LSP commands for:
  " nmap <buffer><silent> <F2> :ALERename<CR>
  " nmap <buffer><silent> gd   :ALEGoToDefinition<CR>
  " nmap <buffer><silent> gy   :ALEGoToTypeDefinition<CR>
  " nmap <buffer><silent> gr   :ALEFindReferences<CR>
  " nmap <buffer><silent> K    :ALEHover<CR>
endfunction

