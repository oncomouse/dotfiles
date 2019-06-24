
function! my_fzf#SearchWordWithAg()
  execute 'Ag' expand('<cword>')
endfunction

function! my_fzf#SearchVisualSelectionWithAg() range
  let old_reg = getreg('"')
  let old_regtype = getregtype('"')
  let old_clipboard = &clipboard
  set clipboard&
  normal! ""gvy
  let selection = getreg('"')
  call setreg('"', old_reg, old_regtype)
  let &clipboard = old_clipboard
  execute 'Ag' selection
endfunction

function! my_fzf#bibtex_cite_sink(lines)
  let r=system("bibtex-cite ", a:lines)
  execute ':normal! i' . r
endfunction

function! my_fzf#bibtex_cite_sink_insert(lines)
  let r=system("bibtex-cite ", a:lines)
  execute ':normal! i' . r
  call feedkeys('a', 'n')
endfunction

function! my_fzf#run_bibtex_ls(sink_command)
  call fzf#run({
    \ 'source': 'bibtex-ls',
    \ 'sink*': function(a:sink_command),
    \ 'up': '40%',
    \ 'options': '--ansi --layout=reverse-list --multi --prompt "Cite> "'})
endfunction
