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
    " Plug 'vim-pandoc/vim-pandoc-syntax', { 'for': ['markdown', 'pandoc'] }
    Plug 'vim-python/python-syntax', { 'for': 'python' }
    Plug 'lepture/vim-css', { 'for': 'css' }
    Plug 'cakebaker/scss-syntax.vim', { 'for': ['sass', 'scss'] }
    Plug 'oncomouse/vim-fish' " Async vim-fish
    Plug 'elzr/vim-json'
  " }}}
  " Web Development: {{{
    Plug 'mattn/emmet-vim', { 'for': ['css', 'elm', 'haml', 'html', 'jade', 'less', 'sass', 'scss', 'slim'] }
    Plug 'alvan/vim-closetag'
  " }}}
  " Python: {{{
    Plug 'tmhedberg/SimpylFold', { 'for': 'python' }
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
  " Use Vim-LSP for LSP:
  Plug 'prabirshrestha/vim-lsp'
  Plug 'mattn/vim-lsp-settings'
  " vim-clap for lists:
  Plug 'liuchengxu/vim-clap', { 'do': ':Clap install-binary!' }
  " Deoplete for completion:
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  Plug 'lighttiger2505/deoplete-vim-lsp' " Adds LSP as a Deoplete source
  Plug 'lionawurscht/deoplete-biblatex' " Adds BibTeX as a Deoplete source
  Plug 'wellle/tmux-complete.vim' " Adds TMUX buffers as a Deoplete source
  " ALE for linting:
  Plug 'dense-analysis/ale'
" }}}
" Writing: {{{
  " Plug 'vim-pandoc/vim-pandoc' " Various pandoc supports
  " Plug 'junegunn/limelight.vim', { 'on': 'Goyo' }
  " Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }
  Plug 'reedes/vim-pencil', { 'for': ['pandoc', 'markdown', 'text'] } " Set various writing-friendly commands
  Plug 'reedes/vim-lexical', { 'for': ['pandoc', 'markdown', 'text'] } " Better spellchecking
  Plug 'reedes/vim-litecorrect' " Add autocorrections for boneheaded typos
  Plug 'reedes/vim-textobj-sentence', { 'for': ['pandoc', 'markdown', 'text'] } " Use as & is for selecting sentences; g) and g( for moving
" }}}
