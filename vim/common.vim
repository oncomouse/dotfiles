" Set <leader> and <localleader>:
let mapleader = "\<Space>"
let maplocalleader = "\\"

let g:bibliography_file = expand('~/Seafile/My Library/Documents/Academic Stuff/library.bib')

set nomodeline
let g:secure_modelines_verbose = 0
let g:secure_modelines_modelines = 15

syntax on

" Basic Vim settings:
set mouse=a
set clipboard=unnamed
set autoread
set vb t_vb=
set autowrite
set hidden " turn off buffer saving when switching

if exists('+termguicolors')
  " let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
  " let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"
  " set termguicolors
endif

" Set default tabs:
set tabstop=8
set shiftwidth=2
set softtabstop=2
set expandtab

" Better (to my mind) split creation defaults:
set splitbelow
set splitright

" Shortcut :tn for :tabnew
ca tn tabnew

" Select whole file
nnoremap <leader>vf ggVG

" Set fold method to syntax by default:
set foldmethod=syntax

" Highlight a block and type "@" to run a macro on the block:
xnoremap @ :<C-u>call visualat#ExecuteMacroOverVisualRange()<CR>

" FastFold:
nmap zuz <Plug>(FastFoldUpdate)
let g:fastfold_savehook = 1
let g:fastfold_fold_command_suffixes =  ['x','X','a','A','o','O','c','C']
let g:fastfold_fold_movement_commands = [']z', '[z', 'zj', 'zk']
let g:fastfold_minlines = 0
