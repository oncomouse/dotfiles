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
  nnoremap <silent> <leader>/ :execute 'Ag ' . input('Ag/')<CR>
  nnoremap <silent> <leader>r :call fzf#run({
    \   'source': 'sed "1d" $HOME/.cache/neomru/file',
    \   'sink': 'e '
    \ })<CR>

  nnoremap <silent> K :call my_fzf#SearchWordWithAg()<CR>
  vnoremap <silent> K :call my_fzf#SearchVisualSelectionWithAg()<CR>
  nnoremap <silent> <leader>gl :Commits<CR>
  nnoremap <silent> <leader>ga :BCommits<CR>
  nnoremap <silent> <leader>ft :Filetypes<CR>

  " Complete file name:
  imap <C-x><C-f> <plug>(fzf-complete-file-ag)
  " Complete file line:
  imap <C-x><C-l> <plug>(fzf-complete-line)
  "
  " FZF BibTeX COnfiguration
  let $FZF_BIBTEX_CACHEDIR = '/var/tmp'
  let $FZF_BIBTEX_SOURCES = g:bibliography_file

  augroup fzf-bibtex
    autocmd!
    " Bind <ctrl+n> to citation look-up using FZF:
    autocmd FileType pandoc,text,markdown nnoremap <silent> <C-N> :call my_fzf#run_bibtex_ls('my_fzf#bibtex_cite_sink')<CR>
    autocmd FileType pandoc,text,markdown inoremap <silent> <C-N> <c-g>u<c-o>:call my_fzf#run_bibtex_ls('my_fzf#bibtex_cite_sink_insert')<CR>
  augroup END
"}}

