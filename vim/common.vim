" Set <leader> and <localleader>:
let mapleader = "\<Space>"
let maplocalleader = "\\"

let g:bibliography_file = expand('~/Seafile/My Library/Documents/Academic Stuff/library.bib')

set nomodeline
let g:secure_modelines_verbose = 0
let g:secure_modelines_modelines = 15

syntax on

set mouse=a
set clipboard=unnamed
set autoread
set vb t_vb=
set autowrite
set hidden " turn off buffer saving when switching

if exists('+termguicolors')
  let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif

set tabstop=8
set shiftwidth=2
set softtabstop=2
set expandtab

set splitbelow
set splitright

" Supposedly, this fixes issues with syntax highlighting:
set redrawtime=10000

" Shortcut :tn for :tabnew
ca tn tabnew
" Select whole file
nnoremap <leader>vf ggVG

" Buffer switching commands:
nnoremap <silent> ]b :bn<CR>
nnoremap <silent> [b :bp<CR>
