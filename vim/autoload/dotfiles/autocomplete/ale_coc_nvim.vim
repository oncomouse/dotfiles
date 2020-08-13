function! dotfiles#autocomplete#ale_coc_nvim#init() abort
  let g:coc_config_home = expand('~/dotfiles/vim/coc.nvim/coc-ale-settings.json')
" Coc Extensions {{{
  " Once your pull request gets accepted, add back coc-go
  let g:coc_global_extensions = [
  \   'coc-bibtex',
  \   'coc-calc',
  \   'coc-css',
  \   'coc-fish',
  \   'coc-html',
  \   'coc-json',
  \   'coc-lists',
  \   'coc-solargraph',
  \   'coc-styled-components',
  \   'coc-tsserver',
  \   'coc-vimlsp',
  \   'coc-yaml',
  \   'coc-yank',
  \]
" }}}
  call dotfiles#autocomplete#coc_nvim#configuration()
  call dotfiles#autocomplete#coc_nvim#keyboard()
  call dotfiles#autocomplete#coc_nvim#fuzzy()
endfunction
