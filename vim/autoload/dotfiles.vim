function! dotfiles#desktop_test() abort
  return g:dotfiles_mode ==# 'desktop'
endfunction
function! dotfiles#has_floating_window() abort
  " MenuPopupChanged was renamed to CompleteChanged -> https://github.com/neovim/neovim/pull/9819
  return (exists('##MenuPopupChanged') || exists('##CompleteChanged')) && exists('*nvim_open_win')
endfunction
function! dotfiles#rg_args(...) abort
  let list = ['-S', '--smart-case', '-i', '--ignore-case', '-w', '--word-regexp',
  \ '-e', '--regexp', '-u', '--unrestricted', '-t', '--type']
  return join(list, "\n")
endfunction
function! dotfiles#install_packages() abort
  packadd vim-packager
  call packager#init({'dir': g:dotfiles_packager_dir})
  call packager#add('kristijanhusak/vim-packager', { 'type': 'opt' })
  call packager#add('https://gitlab.com/protesilaos/tempus-themes-vim') " Theme
  call packager#add('xero/securemodelines') " Secure modelines
  call packager#add('sickill/vim-pasta') " Indentation-forward pasting
  call packager#add('tpope/vim-repeat')
  call packager#add('oncomouse/vim-surround') " ys to add, cs to change, ds to delete. f, F for function, t, T for tag
  call packager#add('tpope/vim-commentary') " Comment w/ gcc or gc (visual)
  call packager#add('airblade/vim-rooter') " Set project root
  call packager#add('tpope/vim-endwise') " Add 'end' to the end of functions
  call packager#add('wellle/targets.vim') " add next block n]) targets, plus words in commas (a,), asterisks (a*), etc
  call packager#add('Raimondi/delimitMate') " Auto-pairing
  if has('nvim-0.5')
    call packager#add('nvim-treesitter/nvim-treesitter')
  else
    call packager#add('machakann/vim-highlightedyank') " Highlights yank
  endif
  call packager#add('vimwiki/vimwiki', { 'type': 'opt' })
  call packager#add('lambdalisue/gina.vim') " :Gina status to schedule; :Gina commit to commit
  if g:dotfiles_mode ==# 'server' || g:complete_package ==# 'fzf'
    call packager#local(isdirectory('/usr/local/opt/fzf') ? '/usr/local/opt/fzf' : '~/.fzf')
    call packager#add('junegunn/fzf.vim') " Add shorcuts for FZF
  endif
  if g:dotfiles_mode ==# 'server'
    call packager#add('sheerun/vim-polyglot')
  elseif g:dotfiles_mode ==# 'desktop'
    if has('nvim')
      " 'ncm2/float-preview.nvim' " Floating preview window
      call packager#add('norcalli/nvim-colorizer.lua') " HTML codes and HTML color words to colors
    endif
    call packager#add('alvan/vim-closetag') " Automatically close HTML tags
    call packager#add('Konfekt/FastFold') " Better fold support
    call packager#add('Yggdroot/indentLine') " Indent with characters
    call packager#add('vim-airline/vim-airline')
    call packager#add('cespare/vim-toml') " Toml syntax
    call packager#add('neoclide/vim-jsx-improve')
    call packager#add('plasticboy/vim-markdown')
    call packager#add('cakebaker/scss-syntax.vim')
    call packager#add('oncomouse/vim-fish') " Async vim-fish
    call packager#add('elzr/vim-json')
    call packager#add('tbastos/vim-lua') " For Lua
    call packager#add('numirias/semshi', { 'do': ':UpdateRemotePlugins' }) " For Python
    call packager#add('kana/vim-textobj-user') " Allow custom textobj definitions
    call packager#add('kana/vim-textobj-function') " af, if, aF, iF select function
    call packager#add('thinca/vim-textobj-function-javascript') " js function support
    call packager#add('bps/vim-textobj-python') " function and class support for Python
    " Need NeoYank for some lists to implement yank history:
    if g:complete_package ==# 'fzf'
      call packager#add('Shougo/neoyank.vim')
      call packager#add('oncomouse/fzf-neoyank') " Add Yank shortcut
    elseif g:complete_package ==# 'clap'
      call packager#add('liuchengxu/vim-clap', { 'do': ':Clap install-binary!' })
    endif
    if g:complete_package !=# 'coc.nvim'
      call packager#add('dense-analysis/ale')
      call packager#add('mhartington/nvim-typescript', {'do': './install.sh'})
      call packager#add('autozimu/LanguageClient-neovim', {
            \ 'branch': 'next',
            \ 'do': 'bash install.sh',
            \ })
      call packager#add('lifepillar/vim-mucomplete')
    else
      call packager#add('neoclide/coc.nvim', {'do': 'yarn install --frozen-lockfile'})
    endif
    call packager#add('godlygeek/tabular') " :Tabular /| to auto-align tables (also :TableFormat in markdown)
    call packager#add('reedes/vim-textobj-sentence', { 'type': 'opt' }) " Use as & is for selecting sentences; g) and g( for moving
    call packager#add('reedes/vim-textobj-quote', { 'type': 'opt' }) " Makes aq & iq for smart quotes
  endif
endfunction
