" Let's Get Started:
Plug 'tpope/vim-sensible' " Good settings
Plug 'https://gitlab.com/protesilaos/tempus-themes-vim.git'
" Plug 'bluz71/vim-moonfly-colors'
" General Editing Plugins:
Plug 'xero/securemodelines' " Secure modelines
" Plug 'sickill/vim-pasta' " Indentation-forward pasting
" Plug 'tpope/vim-surround' " Change surrounding characters
Plug 'machakann/vim-sandwich' " Change surrounding chars, sa adds, sd delete, sr replaces
Plug 'tpope/vim-repeat' " Repeat plugin commands
Plug 'tpope/vim-commentary' " Comment w/ gcc or gc (visual)
Plug 'airblade/vim-rooter' " Set project root
Plug 'tpope/vim-endwise' " Add 'end' to the end of functions
Plug 'wellle/targets.vim' " add next block n]) targets, plus words in commas (a,)
Plug 'jiangmiao/auto-pairs' " Aggressive auto-pairing
Plug 'machakann/vim-highlightedyank' " Highlights yank
" Tmux:
Plug 'christoomey/vim-tmux-navigator' " Navigate TMUX & Vim panes with the same command
" Git Support:
Plug 'lambdalisue/gina.vim' " :Gina status to schedule; :Gina commit to commit
" List Support:
if g:complete_package ==# 'fzf'
  Plug (isdirectory('/usr/local/opt/fzf') ? '/usr/local/opt/fzf' : '~/.fzf')
  Plug 'junegunn/fzf.vim' " Add shorcuts for FZF
endif
