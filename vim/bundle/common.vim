" Let's Get Started:
Plug 'tpope/vim-sensible' " Good settings
" General Editing Plugins:
Plug 'guns/xterm-color-table.vim'
Plug 'xero/securemodelines' " Secure modelines
Plug 'sickill/vim-pasta' " Indentation-forward pasting
Plug 'Yggdroot/indentLine' " Indent with characters
Plug 'tpope/vim-surround' " Change surrounding characters, see cheat sheet
Plug 'tpope/vim-repeat' " Repeat plugin commands
Plug 'tpope/vim-commentary' " Comment w/ gcc or gc (visual)
Plug 'airblade/vim-rooter' " Set project root
Plug 'jiangmiao/auto-pairs' " Aggressive auto-pairing
Plug 'tpope/vim-endwise' " Add 'end' to the end of functions
Plug 'docunext/closetag.vim' " Autoclose HTML tags
Plug 'wellle/targets.vim' " add next block n]) targets, plus words in commas (a,)
" Tmux:
Plug 'christoomey/vim-tmux-navigator' " Navigate TMUX & Vim panes with the same command
" Syntax:
Plug 'sheerun/vim-polyglot'
Plug 'luochen1990/rainbow' " Rainbow parentheses
Plug 'Konfekt/FastFold' " Better fold support
" Fuzzy Search:
if isdirectory(expand('/usr/local/opt/fzf'))
  Plug '/usr/local/opt/fzf' " Load FZF into Vim
else
  Plug '~/.fzf'
endif
Plug 'junegunn/fzf.vim' " Add shorcuts for FZF
" Git Support:
Plug 'lambdalisue/gina.vim' " :Gina status to schedule; :Gina commit to commit
