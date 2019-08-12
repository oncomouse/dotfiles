syntax on

" Basic Vim settings:
set mouse=a " Mouse support
if has('clipboard')
  if has('unnamedplus')
    set clipboard=unnamedplus,unnamed
  else
    set clipboard=unnamed
  endif
endif
set visualbell t_vb= " Disable visual bell
set autowrite " Autosave files
set hidden " turn off buffer saving when switching
set lazyredraw " Don't redraw between macro runs (may make terminal flicker)

" Set default tabs:
set tabstop=8
set shiftwidth=2
set softtabstop=2
set expandtab

" Override default split creation locations:
set splitbelow
set splitright

" Set <leader> and <localleader>:
let mapleader = "\<Space>"
let maplocalleader = "\\"

" Location of BiBLaTeX repo:
let g:bibliography_file = expand('~/Seafile/My Library/Documents/Academic Stuff/library.bib')
" Set this for files to avoid highlighting:
let g:large_file = 20*1024*1024
" Set colorcolumn highlighting for long lines:
let &colorcolumn=join(range(81,999),',')

" Configure securemodeline:
set nomodeline
let g:secure_modelines_verbose = 0
let g:secure_modelines_modelines = 15

" Yanking:
if has('nvim')
  set inccommand=split
endif
" if !exists('##TextYankPost')
"   map y <Plug>(highlightedyank)
" endif

" Disabled Vim Plugins {{{
  let g:loaded_gzip              = 1
  let g:loaded_tar               = 1
  let g:loaded_tarPlugin         = 1
  let g:loaded_zip               = 1
  let g:loaded_zipPlugin         = 1
  let g:loaded_rrhelper          = 1
  let g:loaded_2html_plugin      = 1
  let g:loaded_vimball           = 1
  let g:loaded_vimballPlugin     = 1
  let g:loaded_getscript         = 1
  let g:loaded_getscriptPlugin   = 1
  let g:loaded_netrw             = 1
  let g:loaded_netrwPlugin       = 1
  let g:loaded_netrwSettings     = 1
  let g:loaded_netrwFileHandlers = 1
" }}}
" Maps & Abbreviations {{{
  " Shortcut :tn for :tabnew
  cabbrev tn tabnew
  " Select whole file
  nnoremap <leader>vf ggVG
  " Highlight a block and type "@" to run a macro on the block:
  xnoremap @ :<C-u>call visualat#ExecuteMacroOverVisualRange()<CR>
" }}}
" Autocmds {{{
  augroup dotfile-autocmds
    autocmd BufReadPre *
          \ let s = getfsize(expand("<afile>")) |
          \ if s > g:large_file || s == -2 |
          \   call lf_buffer#large(fnamemodify(expand("<afile>"), ":p")) |
          \ endif
    " On opening a file, jump to the last known cursor position (see :h line())
    autocmd BufReadPost *
          \ if line("'\"") > 1 && line("'\"") <= line("$") && &ft !~# 'commit' |
          \   exe "normal! g`\"" |
          \ endif 
  augroup END
" }}}
