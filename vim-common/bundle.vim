call plug#begin()
" Themes:
Plug 'mhartington/oceanic-next' " Theme
" General Editing Plugins:
Plug 'sickill/vim-pasta' " Indentation-forward pasting
Plug 'editorconfig/editorconfig-vim' " Support editor configuration
Plug 'easymotion/vim-easymotion' " <leader><leader><motion key> for fast motion
Plug 'AndrewRadev/splitjoin.vim' " gS & gJ to split and join one-liners / functions
Plug 'ntpeters/vim-better-whitespace'
Plug 'tpope/vim-surround' " Change surrounding characters, see cheat sheet
Plug 'nathanaelkane/vim-indent-guides' " :IndentGuidesEnable to see indentation guides
Plug 'tpope/vim-repeat' " Repeat plugin commands
Plug 'djoshea/vim-autoread' " Better auto-reloading support
Plug 'MarcWeber/vim-addon-mw-utils' " I think this is required by something else
Plug 'tpope/vim-commentary' " Comment w/ gcc or gc (visual)
Plug 'amix/open_file_under_cursor.vim' " gf to open path at cursor
Plug 'terryma/vim-expand-region' " + or _ to expand or shrink (visual)
Plug 'airblade/vim-rooter' " Set project root
Plug 'rstacruz/vim-closer' " Autoclose brackets etc when pressing Enter
Plug 'tpope/vim-endwise' " Add 'end' to the end of functions
" Plug 'michaeljsmith/vim-indent-object'
" Syntax:
Plug 'sheerun/vim-polyglot' " Multi-language syntax pack (disabled in syntax.vim for pandoc & javascript)
Plug 'othree/yajs.vim' " Support JavaScript syntax (not using vim-polyglot)
Plug 'mxw/vim-jsx' " Modular JSX support to add to yajs
Plug 'othree/es.next.syntax.vim' " Support ES7+ syntax in yajs
Plug 'othree/javascript-libraries-syntax.vim' " Support library syntax in yajs
Plug 'styled-components/vim-styled-components', { 'branch': 'main' } " Styled-components in JS
" Fuzzy Search:
Plug 'Shougo/neomru.vim' " Most Recently Used file list (<leader>r to see
Plug 'brookhong/ag.vim' " :Ag --<filetype> <regex>
Plug '/usr/local/opt/fzf' " Load FZF into Vim
Plug 'junegunn/fzf.vim' " Add shorcuts for FZF
" Plug 'Valloric/YouCompleteMe', { 'do': './install.py --ts-completer --go-completer' }
Plug 'mhartington/nvim-typescript', { 'do': './install.sh'} " Include tsserver support in deoplete
Plug 'deoplete-plugins/deoplete-go', { 'for': 'go', 'do': 'make'} " Add go support for deoplete
Plug 'oncomouse/deoplete-biblatex', { 'for': 'pandoc' } " Custom BibLaTeX source, uses FZF-Bibtex library
Plug 'clojure-vim/async-clj-omni', { 'for': 'clojure' } " Clojure support for deoplete
" Load deoplete differently for NeoVim and Vim:
if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif
" Git Support:
Plug 'tpope/vim-fugitive' " :Gstatus for git status; - to add, cc to commit
Plug 'airblade/vim-gitgutter' " Git stats in the gutter
" Writing:
Plug 'vim-pandoc/vim-pandoc-syntax' " pandoc syntax
Plug 'vim-pandoc/vim-pandoc' " Various pandoc supports
Plug 'junegunn/limelight.vim' " Highlight current paragraph
Plug 'junegunn/goyo.vim' " Minimalist writing environment using :Goyo
Plug 'reedes/vim-pencil' " Set various writing-friendly commands
Plug 'reedes/vim-lexical' " Better spellchecking
Plug 'reedes/vim-litecorrect' " Add autocorrections for boneheaded typos
Plug 'kana/vim-textobj-user' " Allow custom textobj definitions
Plug 'reedes/vim-textobj-sentence' " Use as & is for selecting sentences; g) and g( for moving
Plug 'junegunn/vim-easy-align' " ga will align blocks to indicated character
" Web Development:
Plug 'chrisbra/Colorizer', { 'for': ['html', 'javascript.jsx', 'javascript', 'css', 'scss'] }
Plug 'maksimr/vim-jsbeautify', { 'for': ['javascript', 'javascript.jsx'] }
Plug 'mattn/emmet-vim', { 'for': ['html', 'javascript', 'javascript.jsx' ] }
" File Viewers:
Plug 'ryanoasis/vim-devicons' " Fancy filetype icons in statusbar
" Statusline:
Plug 'itchyny/lightline.vim' " Statusbar
" Plug 'mgee/lightline-bufferline'
" Linter:
Plug 'w0rp/ale' " Linter support
Plug 'maximbaz/lightline-ale' " Add linter status to lightline
" Clojure:
Plug 'tpope/vim-fireplace', { 'for': 'clojure' } " REPL for Clojure
Plug 'kovisoft/paredit' " <leader>> to move parentheses out a layer
Plug 'weavejester/cljfmt', { 'for': 'clojure' } " Formatting for Clojure
Plug 'kien/rainbow_parentheses.vim' " Rainbow parentheses
Plug 'guns/vim-clojure-static', { 'for': 'clojure' }
call plug#end()
