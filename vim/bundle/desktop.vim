" Development: {{{
  " General Syntax: {{{ 
    Plug 'Konfekt/FastFold' " Better fold support
    Plug 'Yggdroot/indentLine' " Indent with characters
    Plug 'luochen1990/rainbow' " Rainbow parentheses
    Plug 'machakann/vim-highlightedyank' " Highlight yanked text
  " }}}
  " Web Syntax: {{{
    Plug 'neoclide/vim-jsx-improve', { 'for': ['javascript', 'javascript.jsx'] }
    Plug 'JulesWang/css.vim', { 'for': 'css' }
  " }}}
  " Other Syntax: {{{
    Plug 'reasonml-editor/vim-reason-plus', { 'for': 'reason' }
    Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries', 'for': 'go' } " Go support
    Plug 'vim-pandoc/vim-pandoc-syntax', { 'for': ['markdown', 'pandoc'] }
    Plug 'vim-python/python-syntax', { 'for': 'python' }
    Plug 'guns/vim-clojure-static', { 'for': 'clojure' }
    Plug 'lepture/vim-css', { 'for': 'css' }
    Plug 'cakebaker/scss-syntax.vim', { 'for': ['sass', 'scss'] }
    Plug 'georgewitteman/vim-fish'
  " }}}
  " Web Development: {{{
    Plug 'chrisbra/Colorizer', { 'for': ['html', 'javascript.jsx', 'javascript', 'css', 'scss'] }
    Plug 'mattn/emmet-vim', { 'for': ['css', 'elm', 'haml', 'html', 'jade', 'less', 'sass', 'scss', 'slim'] }
  " }}}
  " Linter: {{{
    Plug 'w0rp/ale' " Linter support
  " }}}
  " Clojure: {{{
    Plug 'venantius/vim-cljfmt', { 'for': 'clojure' } " Formatting for Clojure
    Plug 'tpope/vim-fireplace', { 'for': 'clojure' } " REPL for Clojure
    Plug 'guns/vim-sexp', { 'for': ['clojure', 'lisp', 'scheme'] } " Better slurp / barf
    Plug 'tpope/vim-sexp-mappings-for-regular-people', { 'for': ['clojure', 'lisp', 'scheme'] } " >) <) >( <) to move parentheses
    Plug 'guns/vim-clojure-highlight', { 'for': 'clojure' } " Clojure support
  " }}}
  " Python: {{{
    Plug 'Vimjas/vim-python-pep8-indent', { 'for': 'python' }
    Plug 'tmhedberg/SimpylFold', { 'for': 'python' }
  " }}}
  " Text Object Plugins: {{{
    Plug 'kana/vim-textobj-user' " Allow custom textobj definitions
    Plug 'kana/vim-textobj-function' " af, if, aF, iF select function
    Plug 'thinca/vim-textobj-function-javascript' " js function support
    Plug 'lucapette/vim-textobj-underscore' " i_, a_ for selecting inside underscores
    Plug 'coderifous/textobj-word-column.vim' " ic, ac, aC, iC column selections
  " }}}
" }}}
" Autocomplete: {{{
  Plug 'autozimu/LanguageClient-neovim', {
      \ 'branch': 'next',
      \ 'do': 'bash install.sh; npm i -g vscode-json-languageserver-bin vscode-css-languageserver-bin vscode-html-languageserver-bin typescript-language-server',
      \ 'for': ['json', 'html', 'css', 'scss', 'sass', 'javascript', 'reason', 'ruby' ],
      \ }
  if has('nvim')
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  else
    Plug 'Shougo/deoplete.nvim'
    Plug 'roxma/nvim-yarp'
    Plug 'roxma/vim-hug-neovim-rpc'
  endif
  Plug 'deoplete-plugins/deoplete-jedi', { 'for': 'python' } " Deoplete source for Python
  Plug 'wellle/tmux-complete.vim' " Read from other Tmux splits
  Plug 'Shougo/neco-vim', { 'for': 'vim' } " Deoplete source for vim
  Plug 'ponko2/deoplete-fish', { 'for': 'fish' } " Deoplete source for fish
" }}}
" Writing: {{{
  Plug 'vim-pandoc/vim-pandoc' " Various pandoc supports
  " Plug 'junegunn/limelight.vim', { 'on': 'Goyo' }
  " Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }
  Plug 'reedes/vim-pencil', { 'for': ['pandoc', 'markdown', 'text'] } " Set various writing-friendly commands
  Plug 'reedes/vim-lexical', { 'for': ['pandoc', 'markdown', 'text'] } " Better spellchecking
  Plug 'reedes/vim-litecorrect' " Add autocorrections for boneheaded typos
  Plug 'reedes/vim-textobj-sentence', { 'for': ['pandoc', 'markdown', 'text'] } " Use as & is for selecting sentences; g) and g( for moving
" }}}
" Statusline:
Plug 'oncomouse/lightline-gitdiff-async'
" Plug '~/Projects/lightline-gitdiff-async'
