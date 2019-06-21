" Set <leader> and <localleader>:
let mapleader = "\<Space>"
let maplocalleader = "\\"

syntax on
filetype plugin indent on

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

" Shortcut :tn for :tabnew
ca tn tabnew

" Include vim-common/config files:
runtime! config/**/*.vim

let g:deoplete#enable_at_startup = 1
let g:nvim_typescript#javascript_support = 1

filetype plugin indent on

set tabstop=8
set shiftwidth=2
set softtabstop=2
set expandtab

set splitbelow
set splitright

" Different highlighting for long lines:
let &colorcolumn=join(range(81,999),",")
highlight ColorColumn ctermbg=235 guibg=#182933

" Spelling colors:
hi clear SpellBad
hi! SpellBad gui=undercurl guibg=#343D46
" hi clear SpellCap
" hi clear SpellRare
" hi! SpellCap gui=undercurl guibg=#C594C5
" hi! SpellRare gui=undercurl guibg=#6699CC
"
" Buffer switching commands:
nnoremap <silent> ]b :bn<CR>
nnoremap <silent> [b :bp<CR>
