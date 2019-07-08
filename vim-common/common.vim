" Set <leader> and <localleader>:
let mapleader = "\<Space>"
let maplocalleader = "\\"

let g:bibliography_file = '/Users/apilsch/Nextcloud/Documents/Academic Stuff/library.bib'

runtime! config/**/*.vim

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
nnoremap <silent> gg :e#<CR>
