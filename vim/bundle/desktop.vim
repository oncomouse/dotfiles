" Development: {{{
  " General Syntax: {{{ 
    Plug 'Konfekt/FastFold' " Better fold support
    Plug 'Yggdroot/indentLine' " Indent with characters
    Plug 'luochen1990/rainbow' " Rainbow parentheses
  " }}}
  " Web Syntax: {{{
    Plug 'neoclide/vim-jsx-improve', { 'for': ['javascript', 'javascript.jsx'] }
    Plug 'JulesWang/css.vim', { 'for': 'css' }
  " }}}
  " Other Syntax: {{{
    Plug 'vim-pandoc/vim-pandoc-syntax', { 'for': ['markdown', 'pandoc'] }
    Plug 'vim-python/python-syntax', { 'for': 'python' }
    Plug 'lepture/vim-css', { 'for': 'css' }
    Plug 'cakebaker/scss-syntax.vim', { 'for': ['sass', 'scss'] }
    Plug 'oncomouse/vim-fish' " Async vim-fish
    Plug 'elzr/vim-json'
  " }}}
  " Web Development: {{{
    Plug 'mattn/emmet-vim', { 'for': ['css', 'elm', 'haml', 'html', 'jade', 'less', 'sass', 'scss', 'slim'] }
  " }}}
  " Python: {{{
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
  Plug 'neoclide/coc.nvim', {'do': { -> coc#util#install()}}
  Plug 'oncomouse/coc-go', {'do': 'yarn install --frozen-lockfile'}
  Plug 'wellle/tmux-complete.vim'
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
