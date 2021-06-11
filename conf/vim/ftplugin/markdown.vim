setlocal wrap linebreak nolist spell
" Turn conceal on and off in a buffer:
function! ToggleConcealLevel() abort
  setlocal conceallevel= &conceallevel == 0 ? 2 : 0
endfunction
nnoremap <buffer> <silent> <leader>cc :call ToggleConcealLevel()<CR>

function! s:get_makeprg() abort
  " Included with vim-rooter:
  let l:root = FindRootDirectory()
  if len(globpath(l:root, '_config.yml'))
    " Jekyll
    let &l:makeprg='bundle exec jekyll build'
  elseif l:root =~# 'slides'
    " Slide Repo
    let &l:makeprg='bundle exec rake build; bundle exec rake deploy'
  else
    " Compile markdown to .docx with pandoc:
    let &l:makeprg='pandoc -f markdown+smart -t docx -i % -o %:r.docx'
  endif
endfunction

setlocal iskeyword+=',-

call s:get_makeprg()

let b:surround_{char2nr('b')} = '**\r**'
let b:surround_{char2nr('l')} = '[\r](\1url: \1)'
