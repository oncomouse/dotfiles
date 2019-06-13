call plug#begin()
" Themes:
Plug 'mhartington/oceanic-next'
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
" Plug 'michaeljsmith/vim-indent-object'
" Syntax:
Plug 'sheerun/vim-polyglot'
Plug 'neoclide/vim-jsx-improve' " Better than what's included
" Plug 'styled-components/vim-styled-components'
" Plug 'leshill/vim-json'
" Plug 'isRuslan/vim-es6'
" Plug 'posva/vim-vue'
" Plug 'mxw/vim-jsx'
" Plug 'pangloss/vim-javascript'
" Plug 'dag/vim-fish'
" Plug 'cakebaker/scss-syntax.vim'
" Plug 'vim-python/python-syntax'
" Fuzzy Search:
" Plug 'ctrlpvim/ctrlp.vim'
Plug 'Shougo/neomru.vim' " Most Recently Used file list (<leader>r to see
Plug 'brookhong/ag.vim' " :Ag --<filetype> <regex>
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
" Git Support:
Plug 'tpope/vim-fugitive' " :Gstatus for git statis; - to add, cc to commit
Plug 'airblade/vim-gitgutter'
" Writing:
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-after' " Adds deoplete completions for BibTeX, use <ctrl>x <ctrl>o to search
Plug 'junegunn/limelight.vim'
Plug 'junegunn/goyo.vim'
Plug 'reedes/vim-pencil'
Plug 'reedes/vim-lexical'
Plug 'reedes/vim-litecorrect'
Plug 'kana/vim-textobj-user'
Plug 'reedes/vim-textobj-sentence'
" Plug 'reedes/vim-textobj-quote'
Plug 'junegunn/vim-easy-align' " ga will align blocks to indicated character
" Web Development:
" Plug 'chrisbra/Colorizer', { "for": ["html", "vue", "jsx", "js", "css", "scss"] }
" Plug 'maksimr/vim-jsbeautify', { "for": ["html", "vue", "jsx", "js", "css", "scss"] }
" Plug 'mattn/emmet-vim', { "for": ["html", "vue", "jsx", "js"] }
" File Viewers:
Plug 'ryanoasis/vim-devicons'
" Plug 'scrooloose/nerdtree'
" Plug 'Xuyuanp/nerdtree-git-plugin'
" Plug 'tpope/vim-vinegar'
" Statusline:
Plug 'itchyny/lightline.vim'
" Plug 'mgee/lightline-bufferline'
" Linter:
" Plug 'w0rp/ale.git'
" Plug 'maximbaz/lightline-ale'
call plug#end()
