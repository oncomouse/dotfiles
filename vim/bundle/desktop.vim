" General Editing: {{{
  Plug 'ncm2/float-preview.nvim' " Floating preview window
  Plug 'norcalli/nvim-colorizer.lua' " HTML codes and HTML color words to colors
  Plug 'alvan/vim-closetag' " Automatically close HTML tags
  Plug 'Konfekt/FastFold' " Better fold support
  Plug 'Yggdroot/indentLine' " Indent with characters
" }}}
" Syntax: {{{
  Plug 'neoclide/vim-jsx-improve'
  Plug 'plasticboy/vim-markdown'
  Plug 'cakebaker/scss-syntax.vim'
  Plug 'oncomouse/vim-fish' " Async vim-fish
  Plug 'elzr/vim-json'
" }}}
" Text Object Plugins: {{{
  Plug 'kana/vim-textobj-user' " Allow custom textobj definitions
  Plug 'kana/vim-textobj-function' " af, if, aF, iF select function
  Plug 'thinca/vim-textobj-function-javascript' " js function support
  Plug 'haya14busa/vim-textobj-function-syntax' " syntax-based function support
  Plug 'lucapette/vim-textobj-underscore' " i_, a_ for selecting inside underscores
  Plug 'coderifous/textobj-word-column.vim' " ic, ac, aC, iC column selections
" }}}
" Autocomplete: {{{
  " Need NeoYank for some lists to implement yank history:
  if g:complete_package ==# 'denite' || g:complete_package ==# 'fzf'
    Plug 'Shougo/neoyank.vim'
  endif
  " Load a list manager:
  if g:complete_package ==# 'clap'
    Plug 'liuchengxu/vim-clap', { 'do': ':Clap install-binary!' }
  elseif g:complete_package ==# 'fzf'
    Plug (isdirectory('/usr/local/opt/fzf') ? '/usr/local/opt/fzf' : '~/.fzf')
    Plug 'junegunn/fzf.vim'
    Plug 'oncomouse/fzf-neoyank' " Add Yank shortcut
  elseif g:complete_package ==# 'denite'
    Plug 'Shougo/denite.nvim', { 'do' : ':UpdateRemotePlugins' }
    Plug 'neoclide/denite-extra' " Adds location_list and quickfix_list as denite sources
  endif
  " Load LSP + Completion:
  if g:complete_package =~# 'coc.nvim'
    Plug 'neoclide/coc.nvim', {'do': 'yarn install --frozen-lockfile'}
  else
    Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
    " Deoplete for completion:
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
    Plug 'lionawurscht/deoplete-biblatex' " Adds BibTeX as a Deoplete source
    Plug 'hrsh7th/vim-neco-calc' " Adds calculator as a Deoplete source
    Plug 'Shougo/neco-vim'
  endif
  " ALE for linting:
  if complete_package !=# 'coc.nvim'
    Plug 'dense-analysis/ale'
  endif
  Plug 'wellle/tmux-complete.vim' " Adds TMUX buffers as a completion source
" }}}
" Writing: {{{
  Plug 'godlygeek/tabular' " :Tabular \| to auto-align tables (also :TableFormat in markdown)
  Plug 'reedes/vim-textobj-sentence' " Use as & is for selecting sentences; g) and g( for moving
" }}}
