" Ack.vim {{
  if executable('ag')
    let g:ackprg = 'ag --vimgrep'
  endif
" }}
" FZF {{
  " Highlight file with <shift>-<tab>; press the follow to open:
  let g:fzf_action = {
      \ 'ctrl-s': 'split',
      \ 'ctrl-v': 'vsplit',
      \ 'ctrl-t': 'tabnew',
      \ 'ctrl-e': 'edit',
      \ }
  nnoremap <silent> <c-p> :FZF<CR>
  nnoremap <silent> <leader>F :FZF ~<CR>
  let g:fzf_nvim_statusline = 0 " disable statusline overwriting

  " Close the quick fix window:
  nnoremap <silent> <leader>c :cclose<CR>

  nnoremap <silent> <leader>a :Buffers<CR>
  nnoremap <silent> <leader>A :Windows<CR>
  nnoremap <silent> <leader>; :BLines<CR>
  nnoremap <silent> <leader>o :BTags<CR>
  nnoremap <silent> <leader>O :Tags<CR>
  nnoremap <silent> <leader>? :History<CR>
  nnoremap <silent> <leader>/ :execute 'Ack! ' . input('Ack/')<CR>
  nnoremap <silent> <leader>r :call fzf#run({
    \   'source': 'sed "1d" $HOME/.cache/neomru/file',
    \   'sink': 'e '
    \ })<CR>

  nnoremap <silent> K :call SearchWordWithAg()<CR>
  vnoremap <silent> K :call SearchVisualSelectionWithAg()<CR>
  nnoremap <silent> <leader>gl :Commits<CR>
  nnoremap <silent> <leader>ga :BCommits<CR>
  nnoremap <silent> <leader>ft :Filetypes<CR>

  " Complete file name:
  imap <C-x><C-f> <plug>(fzf-complete-file-ag)
  " Complete file line:
  imap <C-x><C-l> <plug>(fzf-complete-line)

  function! SearchWordWithAg()
    execute 'Ack!' expand('<cword>')
  endfunction

  function! SearchVisualSelectionWithAg() range
    let old_reg = getreg('"')
    let old_regtype = getregtype('"')
    let old_clipboard = &clipboard
    set clipboard&
    normal! ""gvy
    let selection = getreg('"')
    call setreg('"', old_reg, old_regtype)
    let &clipboard = old_clipboard
    execute 'Ack!' selection
  endfunction
  "
  " FZF BibTeX COnfiguration
  let $FZF_BIBTEX_CACHEDIR = '/var/tmp'
  let $FZF_BIBTEX_SOURCES = '/Users/apilsch/Dropbox/Documents/Academic Stuff/library.bib'

  function! s:bibtex_cite_sink(lines)
    let r=system("bibtex-cite ", a:lines)
    execute ':normal! i' . r
  endfunction

  function! s:bibtex_cite_sink_insert(lines)
    let r=system("bibtex-cite ", a:lines)
    execute ':normal! i' . r
    call feedkeys('a', 'n')
  endfunction

  augroup fzf-bibtex
    autocmd!
    autocmd FileType pandoc,text,markdown nnoremap <silent> <C-c> :call fzf#run({
                \ 'source': 'bibtex-ls',
                \ 'sink*': function('<sid>bibtex_cite_sink'),
                \ 'up': '40%',
                \ 'options': '--ansi --layout=reverse-list --multi --prompt "Cite> "'})<CR>

    autocmd FileType pandoc,text,markdown inoremap <silent> <C-c> <c-g>u<c-o>:call fzf#run({
              \ 'source': 'bibtex-ls',
              \ 'sink*': function('<sid>bibtex_cite_sink_insert'),
              \ 'up': '40%',
              \ 'options': '--ansi --layout=reverse-list --multi --prompt "Cite> "'})<CR>
  augroup END
"}}

