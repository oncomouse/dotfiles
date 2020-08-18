" General Editing: {{{
  Plug 'ncm2/float-preview.nvim' " Floating preview window
" }}}
" Development: {{{
  " General Syntax: {{{ 
    Plug 'Konfekt/FastFold' " Better fold support
    Plug 'Yggdroot/indentLine' " Indent with characters
  " }}}
  " Web Syntax: {{{
    Plug 'neoclide/vim-jsx-improve', { 'for': ['javascript', 'javascriptreact'] }
    Plug 'JulesWang/css.vim', { 'for': 'css' }
    Plug 'norcalli/nvim-colorizer.lua' " Colorizer
  " }}}
  " Other Syntax: {{{
    Plug 'plasticboy/vim-markdown'
    Plug 'lepture/vim-css'
    Plug 'cakebaker/scss-syntax.vim'
    Plug 'oncomouse/vim-fish' " Async vim-fish
    Plug 'elzr/vim-json'
  " }}}
  " Web Development: {{{
    Plug 'alvan/vim-closetag'
  " }}}
  " Text Object Plugins: {{{
    Plug 'kana/vim-textobj-user' " Allow custom textobj definitions
    Plug 'kana/vim-textobj-function' " af, if, aF, iF select function
    Plug 'thinca/vim-textobj-function-javascript' " js function support
    Plug 'haya14busa/vim-textobj-function-syntax' " syntax function support
    Plug 'lucapette/vim-textobj-underscore' " i_, a_ for selecting inside underscores
    Plug 'coderifous/textobj-word-column.vim' " ic, ac, aC, iC column selections
  " }}}
" }}}
" Autocomplete: {{{
  " Load a list manager:
  if g:complete_package ==# 'denite' || g:complete_package ==# 'fzf'
    Plug 'Shougo/neoyank.vim'
  endif
  if g:complete_package ==# 'clap'
    " vim-clap for lists:
    Plug 'liuchengxu/vim-clap', { 'do': ':Clap install-binary!' }
  elseif g:complete_package ==# 'fzf'
    Plug (isdirectory('/usr/local/opt/fzf') ? '/usr/local/opt/fzf' : '~/.fzf')
    Plug 'junegunn/fzf.vim' " Add shorcuts for FZF
    Plug 'oncomouse/fzf-neoyank' " Add Yank shortcut
  elseif g:complete_package ==# 'denite'
    Plug 'Shougo/denite.nvim', { 'do' : ':UpdateRemotePlugins' }
    Plug 'neoclide/denite-extra' " Adds location_list as a denite source
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
  endif
  " ALE for linting:
  if complete_package !=# 'coc.nvim'
    Plug 'dense-analysis/ale'
  endif
  Plug 'wellle/tmux-complete.vim' " Adds TMUX buffers as a Deoplete source
" }}}
" Writing: {{{
  Plug 'godlygeek/tabular' " :Tabular \| to auto-align tables
  Plug 'reedes/vim-litecorrect' " Add autocorrections for boneheaded typos
  Plug 'reedes/vim-textobj-sentence' " Use as & is for selecting sentences; g) and g( for moving
" }}}
