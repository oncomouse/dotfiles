function! fzf_bibtex#bibtex_cite_sink(lines)
  let r=system("bibtex-cite ", a:lines)
  execute ':normal! i' . r
endfunction

function! fzf_bibtex#bibtex_cite_sink_insert(lines)
  let r=system("bibtex-cite ", a:lines)
  execute ':normal! i' . r
  call feedkeys('a', 'n')
endfunction

function! fzf_bibtex#bibtex_markdown_sink(lines)
  let r=system("bibtex-markdown ", a:lines)
  execute ':normal! i' . r
endfunction

function! fzf_bibtex#bibtex_markdown_sink_insert(lines)
  let r=system("bibtex-markdown ", a:lines)
  execute ':normal! i' . r
  call feedkeys('a', 'n')
endfunction

function! fzf_bibtex#run_bibtex_ls(sink_command)
  call fzf#run({
    \ 'source': 'bibtex-ls',
    \ 'sink*': function(a:sink_command),
    \ 'up': '40%',
    \ 'options': '--ansi --layout=reverse-list --multi --prompt "Cite> "'})
endfunction
