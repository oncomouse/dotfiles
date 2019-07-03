call plug#begin()
" Themes:
Plug 'mhartington/oceanic-next' " Theme
" General Editing Plugins:
Plug 'jiangmiao/auto-pairs' " Autocomplete pairable marks
Plug 'sickill/vim-pasta' " Indentation-forward pasting
Plug 'easymotion/vim-easymotion' " <leader><leader><motion key> for fast motion
Plug 'AndrewRadev/splitjoin.vim' " gS & gJ to split and join one-liners / functions
Plug 'ntpeters/vim-better-whitespace'
Plug 'tpope/vim-surround' " Change surrounding characters, see cheat sheet
Plug 'nathanaelkane/vim-indent-guides' " :IndentGuidesEnable to see indentation guides
Plug 'tpope/vim-repeat' " Repeat plugin commands
Plug 'djoshea/vim-autoread' " Better auto-reloading support
Plug 'tpope/vim-commentary' " Comment w/ gcc or gc (visual)
Plug 'amix/open_file_under_cursor.vim' " gf to open path at cursor
Plug 'terryma/vim-expand-region' " + or _ to expand or shrink (visual)
Plug 'airblade/vim-rooter' " Set project root
Plug 'rstacruz/vim-closer' " Autoclose brackets etc when pressing Enter
Plug 'tpope/vim-endwise' " Add 'end' to the end of functions
Plug 'docunext/closetag.vim' " Do the same for HTML
Plug 'christoomey/vim-tmux-navigator' " Navigate TMUX & Vim panes with the same command
" Web Syntax:
Plug 'hail2u/vim-css3-syntax', { 'for': 'css' } " Somehow, polyglot doesn't load this.
Plug 'MaxMEllon/vim-jsx-pretty', { 'for': ['javascript', 'javascript.jsx'] } " Modular JSX support to add to yajs
Plug 'othree/yajs.vim', { 'for': ['javascript', 'javascript.jsx'] } " Support JavaScript syntax (not using vim-polyglot)
Plug 'othree/es.next.syntax.vim', { 'for': ['javascript', 'javascript.jsx'] } " Support ES7+ syntax in yajs
Plug 'othree/javascript-libraries-syntax.vim', { 'for': ['javascript', 'javascript.jsx'] } " Support library syntax in yajs
Plug 'styled-components/vim-styled-components', { 'branch': 'main' } " Styled-components in JS
Plug 'cakebaker/scss-syntax.vim', { 'for': 'scss' } " SCSS
" Other Syntax:
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries', 'for': 'go' } " Go support
Plug 'guns/vim-clojure-highlight', { 'for': 'clojure' } " Clojure support
Plug 'dag/vim-fish', { 'for': 'fish' }
Plug 'ericpruitt/tmux.vim', {'rtp': 'vim/'}
Plug 'vim-python/python-syntax', { 'for': 'python' }
Plug 'Vimjas/vim-python-pep8-indent', { 'for': 'python' }
Plug 'stephpy/vim-yaml', { 'for': 'yaml' } " YAML
" Plug 'chr4/nginx.vim' " activate if you need this
Plug 'chrisbra/csv.vim', { 'for': 'csv' }
Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
" Fuzzy Search:
Plug 'Shougo/neomru.vim' " Most Recently Used file list (<leader>r to see
Plug '/usr/local/opt/fzf' " Load FZF into Vim
Plug 'junegunn/fzf.vim' " Add shorcuts for FZF
Plug 'neoclide/coc.nvim', {'branch': 'release'} " Use for code completion and snippet triggering
Plug 'Shougo/neosnippet.vim' " Snippets support
Plug 'Shougo/neosnippet-snippets' " The snippets
" Git Support:
Plug 'skywind3000/asyncrun.vim' " Does a lot of stuff, but we use it to make vim-fugitive async
Plug 'tpope/vim-fugitive' " :Gstatus for git status; - to add, cc to commit
Plug 'lambdalisue/gina.vim' " :Gina status to schedule; :Gina commit to commit
Plug 'airblade/vim-gitgutter' " Git stats in the gutter
" Writing:
Plug 'vim-pandoc/vim-pandoc-syntax' " pandoc syntax
Plug 'vim-pandoc/vim-pandoc' " Various pandoc supports
" Plug 'junegunn/limelight.vim', { 'for': ['pandoc', 'markdown', 'text'] } " Highlight current paragraph
Plug 'junegunn/goyo.vim', { 'for': ['pandoc', 'markdown', 'text'] } " Minimalist writing environment using :Goyo
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
" Linter:
Plug 'w0rp/ale' " Linter support
Plug 'maximbaz/lightline-ale' " Add linter status to lightline
" Plug 'oncomouse/lightline-coc/'
" Clojure:
Plug 'venantius/vim-cljfmt', { 'for': 'clojure' } " Formatting for Clojure
Plug 'tpope/vim-fireplace', { 'for': 'clojure' } " REPL for Clojure
Plug 'guns/vim-sexp' " Better slurp / barf
Plug 'tpope/vim-sexp-mappings-for-regular-people' " >) <) >( <) to move parentheses
Plug 'luochen1990/rainbow' " Rainbow parentheses
Plug 'guns/vim-clojure-static', { 'for': 'clojure' }
call plug#end()
