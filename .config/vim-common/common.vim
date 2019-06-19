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
let g:vim_home = '~/.config/vim-common/'
let g:editor_root=expand('~/.config/vim-common/')
let s:config_list = [
  \ 'config/theme.vim',
  \ 'config/statusline.vim',
  \ 'config/basic.vim',
  \ 'config/syntax.vim',
  \ 'config/fuzzy.vim',
  \ 'config/autocomplete.vim',
  \ 'config/git.vim',
  \ 'config/writing.vim',
  \ 'config/webdev.vim',
  \ 'config/filebrowser.vim',
  \ 'config/visual-at.vim',
  \ 'config/linter.vim',
  \ 'config/todo.vim',
\]

for files in s:config_list
  for f in split(glob(vim_home.files), '\n')
    exec 'source '.f
  endfor
endfor

filetype plugin indent on
set tabstop=8
set shiftwidth=2
set softtabstop=2
set expandtab

" Different highlighting for long lines:
let &colorcolumn=join(range(81,999),",")
highlight ColorColumn ctermbg=235 guibg=#182933
