" CTRL-P {{
  " ctrlp support
  " let g:ctrlp_brief_prompt=1
  " let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git'
  " let g:ctrlp_show_hidden = 1
  " let g:ctrlp_prompt_mappings = {
  "     \ 'AcceptSelection("h")': ['<c-x>', '<c-s>'],
  "     \ 'AcceptSelection("e")': ['<c-t>',],
  "     \ 'AcceptSelection("t")': ['<cr>', '<2-LeftMouse>'],
  "     \ }
"}}
" FZF {{

  nnoremap <c-p> :FZF<cr>
  let g:fzf_nvim_statusline = 0 " disable statusline overwriting

  nnoremap <silent> <leader>f :Files<CR>
  nnoremap <silent> <leader>a :Buffers<CR>
  nnoremap <silent> <leader>A :Windows<CR>
  nnoremap <silent> <leader>; :BLines<CR>
  nnoremap <silent> <leader>o :BTags<CR>
  nnoremap <silent> <leader>O :Tags<CR>
  nnoremap <silent> <leader>? :History<CR>
  nnoremap <silent> <leader>/ :execute 'Ag ' . input('Ag/')<CR>
  nnoremap <silent> <leader>. :AgIn
  nnoremap <silent> <leader>r :call fzf#run({
    \   'source': 'sed "1d" $HOME/.cache/neomru/file',
    \   'sink': 'e '
    \ })<CR>

  nnoremap <silent> K :call SearchWordWithAg()<CR>
  vnoremap <silent> K :call SearchVisualSelectionWithAg()<CR>
  nnoremap <silent> <leader>gl :Commits<CR>
  nnoremap <silent> <leader>ga :BCommits<CR>
  nnoremap <silent> <leader>ft :Filetypes<CR>

  " FZF BibTeX COnfiguration
  let $FZF_BIBTEX_CACHEDIR = '/var/tmp'
  let $FZF_BIBTEX_SOURCES = '/Users/apilsch/Dropbox/Documents/Academic Stuff/library.bib'
  function! s:bibtex_cite_sink(lines)
    let r=system("bibtex-cite ", a:lines)
    execute ':normal! i' . r
  endfunction

  nnoremap <C-c> :call fzf#run({
              \ 'source': 'bibtex-ls',
              \ 'sink*': function('<sid>bibtex_cite_sink'),
              \ 'up': '40%',
              \ 'options': '--ansi --layout=reverse-list --multi --prompt "Cite> "'})<CR>

  function! s:bibtex_cite_sink_insert(lines)
    let r=system("bibtex-cite ", a:lines)
    execute ':normal! i' . r
    call feedkeys('a', 'n')
  endfunction

  inoremap <silent> <C-c> <c-g>u<c-o>:call fzf#run({
            \ 'source': 'bibtex-ls',
            \ 'sink*': function('<sid>bibtex_cite_sink_insert'),
            \ 'up': '40%',
            \ 'options': '--ansi --layout=reverse-list --multi --prompt "Cite> "'})<CR>

  imap <C-x><C-f> <plug>(fzf-complete-file-ag)
  imap <C-x><C-l> <plug>(fzf-complete-line)

  function! SearchWordWithAg()
    execute 'Ag' expand('<cword>')
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
    execute 'Ag' selection
  endfunction

  function! SearchWithAgInDirectory(...)
    call fzf#vim#ag(join(a:000[1:], ' '), extend({'dir': a:1}, g:fzf#vim#default_layout))
  endfunction
  command! -nargs=+ -complete=dir AgIn call SearchWithAgInDirectory(<f-args>)
"}}

