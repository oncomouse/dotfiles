function! dotfiles#fzf#bibtex_cite_sink(lines) abort
  let r=system('bibtex-cite ', a:lines)
  execute ':normal! i' . r
endfunction

function! dotfiles#fzf#bibtex_cite_sink_insert(lines) abort
  let r=system('bibtex-cite ', a:lines)
  execute ':normal! i' . r
  call feedkeys('a', 'n')
endfunction

function! dotfiles#fzf#bibtex_markdown_sink(lines) abort
  let r=system('bibtex-markdown ', a:lines)
  execute ':normal! i' . r
endfunction

function! dotfiles#fzf#bibtex_markdown_sink_insert(lines) abort
  let r=system('bibtex-markdown ', a:lines)
  execute ':normal! i' . r
  call feedkeys('a', 'n')
endfunction

function! dotfiles#fzf#bibtex_run_ls(sink_command) abort
  call fzf#run({
    \ 'source': 'bibtex-ls',
    \ 'sink*': function(a:sink_command),
    \ 'up': '40%',
    \ 'options': '--ansi --layout=reverse-list --multi --prompt "Cite> "'})
endfunction
