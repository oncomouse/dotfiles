" Set <leader> and <localleader>:
let mapleader = "\<Space>"
let maplocalleader = "\\"

let &runtimepath .= ','.expand('~/dotfiles/vim-common/after/')

syntax on

set mouse=a
set clipboard=unnamed
set autoread
set autowrite
set hidden " turn off buffer saving when switching

if exists('+termguicolors')
  let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif

filetype plugin indent on

set tabstop=8
set shiftwidth=2
set softtabstop=2
set expandtab

set splitbelow
set splitright

let g:bibliography_file = '/Users/apilsch/Dropbox/Documents/Academic Stuff/library.bib'

" Shortcut :tn for :tabnew
ca tn tabnew
" Select whole file
nnoremap <leader>vf ggVG
