function! dotfiles#autocomplete#coc_nvim#init() abort
" Coc Extensions {{{
  let g:coc_global_extensions = [
  \   'coc-bibtex',
  \   'coc-calc',
  \   'coc-css',
  \   'coc-diagnostic',
  \   'coc-eslint',
  \   'coc-fish',
  \   'coc-html',
  \   'coc-json',
  \   'coc-lists',
  \   'coc-prettier',
  \   'coc-solargraph',
  \   'coc-stylelintplus',
  \   'coc-styled-components',
  \   'coc-tsserver',
  \   'coc-vimlsp',
  \   'coc-yaml',
  \   'coc-yank',
  \]
" }}}
" Coc Configuration {{{
  " Coc Floating Window Support:
  let g:coc_config_home = expand('~/dotfiles/vim/coc.nvim/coc-settings.json')
  call coc#config('coc.preferences', {
      \ 'hoverTarget': dotfiles#has_floating_window() ? 'float' : 'echo',
      \ })
  call coc#config('suggest', {
      \ 'echodocSupport': 1,
      \ 'floatEnable': dotfiles#has_floating_window(),
      \ })
  call coc#config('signature', {
      \ 'target': dotfiles#has_floating_window() ? 'float' : 'echo',
      \ })
  call coc#config('diagnostics', {
      \ 'messageTarget': dotfiles#has_floating_window() ? 'float' : 'echo',
      \ })
" }}}
" Coc Keyboard shortcuts: {{{
  function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
      execute 'h '.expand('<cword>')
    else
      call CocAction('doHover')
    endif
  endfunction
  imap <expr><TAB> pumvisible() ? "\<C-n>" : dotfiles#smart_tab()
  imap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<S-TAB>"
  nmap <silent> <F2> <Plug>(coc-rename)
  nmap <silent> gd <Plug>(coc-definition)
  nmap <silent> gy <Plug>(coc-type-definition)
  nmap <silent> gi <Plug>(coc-implementation)
  nmap <silent> gr <Plug>(coc-references)
  nmap <silent> ]d <Plug>(coc-diagnostic-next)
  nmap <silent> [d <Plug>(coc-diagnostic-prev)
  nmap <silent> <leader>d :<C-u>CocList loclist<CR>
  nnoremap <silent> K :call <SID>show_documentation()<CR>
  nmap <leader>r <Plug>(coc-rename)
  command! Symbols :<C-u>CocList -I symbols<cr>
  nmap <leader>s :Symbols<CR>
  " append result on current expression
  nmap <Leader>ca <Plug>(coc-calc-result-append)
  " replace result on current expression
  nmap <Leader>cr <Plug>(coc-calc-result-replace)
" }}}
" Coc Fuzzy {{{
  " (Implement fzf.vim lists for CocList)
  let s:is_win = has('win32') || has('win64')
  function! s:shortpath()
    let short = fnamemodify(getcwd(), ':~:.')
    if !has('win32unix')
      let short = pathshorten(short)
    endif
    let slash = (s:is_win && !&shellslash) ? '\' : '/'
    return empty(short) ? '~'.slash : short . (short =~ escape(slash, '\').'$' ? '' : slash)
  endfunction

  function! CocFiles(dir, ...)
    let args = {}
    if !empty(a:dir)
      if !isdirectory(expand(a:dir))
        echohl WarningMsg
        echom 'Invalid directory'
        echohl None
      endif
      let slash = (s:is_win && !&shellslash) ? '\\' : '/'
      let dir = substitute(a:dir, '[/\\]*$', slash, '')
      let args.dir = dir
      exe 'CocList files '.expand(dir)
    else
      exe 'CocList files'
    endif
  endfunction
  command!      -bang -nargs=? -complete=dir FZF       call CocFiles(<q-args>, <bang>0)
  command! Buffers :exe 'CocList buffers'
  command! Windows :exe 'CocList windows'
  command! BLines :exe 'CocList lines'
  command! History :exe 'CocList cmdhistory'
  nnoremap <silent> <leader>y  :<C-u>CocList -A --normal yank<cr>
  " Implement Ag
  command! -nargs=+ -complete=custom,s:GrepArgs Ag exe 'CocList grep '.<q-args>
"}}}
endfunction

function! dotfiles#autocomplete#coc_nvim#writing() abort
  call coc#config('list.source.bibtex', {
  \  'files': [
  \    g:bibliography_file,
  \  ]
  \})
  augroup coc-bibtex
    autocmd!
    autocmd FileType pandoc,markdown nnoremap <silent> <C-C> :execute 'CocList bibtex'<CR>
    autocmd FileType pandoc,markdown inoremap <silent> <C-C> <c-g>u<c-o>:execute 'CocList bibtex'<CR>
  augroup END
    augroup coc-pandoc
    autocmd!
    autocmd FileType markdown,pandoc call coc#config('coc.source.buffer.enable', 0)
    autocmd FileType markdown,pandoc call coc#config('coc.source.around.enable', 0)
    autocmd FileType markdown,pandoc call coc#config('coc.source.snippets.enable', 0)
    autocmd FileType markdown,pandoc call coc#config('coc.source.file.enable', 0)
    autocmd FileType markdown,pandoc call coc#config('coc.source.tmux.enable', 0)
  augroup END
  function! CocBufferOn() abort
    call coc#config('coc.source.buffer.enable', 1)
    call coc#config('coc.source.around.enable', 1)
  endfunction
  function! CocBufferOff() abort
    call coc#config('coc.source.buffer.enable', 0)
    call coc#config('coc.source.around.enable', 0)
  endfunction
  command! CocBufferOn call CocBufferOn()
  command! CocBufferOff call CocBufferOff()
endfunction
