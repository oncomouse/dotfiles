function! dotfiles#autocomplete#deoplete#init() abort
endfunction
function! dotfiles#autocomplete#deoplete#writing() abort
  let g:deoplete#sources#biblatex#bibfile = g:bibliography_file
  let g:deoplete#sources#biblatex#addinfo = v:true
  call deoplete#custom#source('biblatex', 'filetypes', ['markdown'])
  call deoplete#custom#option('ignore_sources', {'markdown': ['around', 'buffer', 'file', 'tmux']})
endfunction
