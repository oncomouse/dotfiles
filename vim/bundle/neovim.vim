" Web Syntax:
Plug 'hail2u/vim-css3-syntax', { 'for': 'css' } " Somehow, polyglot doesn't load this.
Plug 'MaxMEllon/vim-jsx-pretty', { 'for': ['javascript', 'javascript.jsx'] } " Modular JSX support to add to yajs
Plug 'othree/yajs.vim', { 'for': ['javascript', 'javascript.jsx'] } " Support JavaScript syntax (not using vim-polyglot)
Plug 'othree/es.next.syntax.vim', { 'for': ['javascript', 'javascript.jsx'] } " Support ES7+ syntax in yajs
Plug 'othree/javascript-libraries-syntax.vim', { 'for': ['javascript', 'javascript.jsx'] } " Support library syntax in yajs
" Plug 'styled-components/vim-styled-components', { 'branch': 'main' } " Styled-components in JS
Plug 'cakebaker/scss-syntax.vim', { 'for': 'scss' } " SCSS
" Other Syntax:
Plug 'reasonml-editor/vim-reason-plus'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries', 'for': 'go' } " Go support
Plug 'guns/vim-clojure-highlight', { 'for': 'clojure' } " Clojure support
Plug 'georgewitteman/vim-fish', { 'for': 'fish' }
Plug 'ericpruitt/tmux.vim', {'rtp': 'vim/'}
Plug 'vim-python/python-syntax', { 'for': 'python' }
Plug 'Vimjas/vim-python-pep8-indent', { 'for': 'python' }
Plug 'stephpy/vim-yaml', { 'for': 'yaml' } " YAML
" Plug 'chr4/nginx.vim' " activate if you need this
Plug 'chrisbra/csv.vim', { 'for': 'csv' }
Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
" Autocomplete:
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh; npm i -g vscode-json-languageserver-bin vscode-css-languageserver-bin vscode-html-languageserver-bin typescript-language-server',
    \ }
Plug 'deoplete-plugins/deoplete-jedi', { 'for': ['python'] }
Plug 'Shougo/neco-syntax'
Plug 'Shougo/neco-vim'
Plug 'wellle/tmux-complete.vim'
" Writing:
Plug 'vim-pandoc/vim-pandoc-syntax' " pandoc syntax
Plug 'vim-pandoc/vim-pandoc' " Various pandoc supports
Plug 'junegunn/limelight.vim', { 'on': 'Goyo' }
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }
Plug 'reedes/vim-pencil', { 'for': ['pandoc', 'markdown', 'text'] } " Set various writing-friendly commands
Plug 'reedes/vim-lexical', { 'for': ['pandoc', 'markdown', 'text'] } " Better spellchecking
Plug 'reedes/vim-litecorrect' " Add autocorrections for boneheaded typos
Plug 'kana/vim-textobj-user' " Allow custom textobj definitions
Plug 'reedes/vim-textobj-sentence', { 'for': ['pandoc', 'markdown', 'text'] } " Use as & is for selecting sentences; g) and g( for moving
Plug 'junegunn/vim-easy-align' " ga will align blocks to indicated character
" Web Development:
Plug 'chrisbra/Colorizer', { 'for': ['html', 'javascript.jsx', 'javascript', 'css', 'scss'] }
Plug 'maksimr/vim-jsbeautify', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'mattn/emmet-vim', { 'for': ['html', 'javascript', 'javascript.jsx' ] }
" File Viewers:
Plug 'ryanoasis/vim-devicons' " Fancy filetype icons in statusbar
" Statusline:
Plug 'itchyny/lightline.vim' " Statusbar
Plug 'Palpatineli/lightline-lsc-nvim'
Plug 'sinetoami/lightline-neomake'
" Linter:
Plug 'neomake/neomake'
Plug 'benjie/local-npm-bin.vim'
" Plug 'w0rp/ale' " Linter support
" Plug 'maximbaz/lightline-ale' " Add linter status to lightline
" Clojure:
Plug 'venantius/vim-cljfmt', { 'for': 'clojure' } " Formatting for Clojure
Plug 'tpope/vim-fireplace', { 'for': 'clojure' } " REPL for Clojure
Plug 'guns/vim-clojure-static', { 'for': 'clojure' }
Plug 'guns/vim-sexp', { 'for': ['clojure', 'lisp', 'scheme'] } " Better slurp / barf
Plug 'tpope/vim-sexp-mappings-for-regular-people', { 'for': ['clojure', 'lisp', 'scheme'] } " >) <) >( <) to move parentheses
