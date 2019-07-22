" Themes:
Plug 'mhartington/oceanic-next' " Theme
" General Editing Plugins:
Plug 'xero/securemodelines' " Secure modelines
Plug 'sickill/vim-pasta' " Indentation-forward pasting
Plug 'easymotion/vim-easymotion' " <leader><leader><motion key> for fast motion
" Plug 'ntpeters/vim-better-whitespace'
Plug 'tpope/vim-surround' " Change surrounding characters, see cheat sheet
Plug 'nathanaelkane/vim-indent-guides' " :IndentGuidesEnable to see indentation guides
Plug 'tpope/vim-repeat' " Repeat plugin commands
Plug 'tpope/vim-commentary' " Comment w/ gcc or gc (visual)
" Plug 'terryma/vim-expand-region' " + or _ to expand or shrink (visual)
Plug 'airblade/vim-rooter' " Set project root
Plug 'jiangmiao/auto-pairs' " Aggressive auto-pairing (may use only in certain languages)
Plug 'tpope/vim-endwise' " Add 'end' to the end of functions
Plug 'docunext/closetag.vim' " Autoclose HTML tags
" Tmux:
Plug 'christoomey/vim-tmux-navigator' " Navigate TMUX & Vim panes with the same command
" Syntax:
Plug 'luochen1990/rainbow' " Rainbow parentheses
" Fuzzy Search:
if isdirectory(expand('/usr/local/opt/fzf'))
  Plug '/usr/local/opt/fzf' " Load FZF into Vim
else
  Plug '~/.fzf'
endif
Plug 'junegunn/fzf.vim' " Add shorcuts for FZF
Plug 'Shougo/neoyank.vim'
Plug 'justinhoward/fzf-neoyank'
" Git Support:
Plug 'lambdalisue/gina.vim' " :Gina status to schedule; :Gina commit to commit
Plug 'airblade/vim-gitgutter' " Git stats in the gutter
