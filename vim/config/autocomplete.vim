" Coc Extensions {{{
  " Once your pull request gets accepted, add back coc-go
  let g:coc_global_extensions = [
  \   'coc-bibtex',
  \   'coc-calc',
  \   'coc-css',
  \   'coc-diagnostic',
  \   'coc-fish',
  \   'coc-go',
  \   'coc-html',
  \   'coc-json',
  \   'coc-lists',
  \   'coc-pairs',
  \   'coc-python',
  \   'coc-solargraph',
  \   'coc-styled-components',
  \   'coc-tsserver',
  \   'coc-vimlsp',
  \   'coc-yaml',
  \   'coc-yank',
  \]
" }}}
" Coc Configuration {{{
  " Coc Floating Window Support:
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
  nmap <silent> gd <Plug>(coc-definition)
  nmap <silent> gy <Plug>(coc-type-definition)
  nmap <silent> gi <Plug>(coc-implementation)
  nmap <silent> gr <Plug>(coc-references)
  nmap <silent> ]d <Plug>(coc-diagnostic-next)
  nmap <silent> [d <Plug>(coc-diagnostic-prev)
  nmap <silent> []d :<C-u>CocList diagnostics<CR>
  nmap <silent> <leader>lk <Plug>(coc-diagnostic-prev)
  nmap <silent> <leader>lj <Plug>(coc-diagnostic-next)
  nmap <silent> <leader>ll :<C-u>CocList diagnostics<CR>
  nnoremap <silent> K :call <SID>show_documentation()<CR>
  nmap <leader>rn <Plug>(coc-rename)
  command! Symbols :<C-u>CocList -I symbols<cr>
  nmap <leader>s :Symbols<CR>
  " append result on current expression
  nmap <Leader>ca <Plug>(coc-calc-result-append)
  " replace result on current expression
  nmap <Leader>cr <Plug>(coc-calc-result-replace)
" }}}
" Coc Formatting {{{
  set formatexpr=CocAction('formatSelected')
  xmap <leader>f  <Plug>(coc-format-selected)
  nmap <leader>f  <Plug>(coc-format-selected)
  command! -nargs=0 Format :call CocAction('format')
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
        echom s:warn('Invalid directory')
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
  nnoremap <silent> <leader>rr  :<C-u>CocList -A --normal yank<cr>
  " Implement Ag
  " call coc#config('list.source.grep.command', 'ag')
  command! -nargs=+ -complete=custom,s:GrepArgs Ag exe 'CocList grep '.<q-args>

" }}}
" Coc Filetypes {{{
  " Go {{{
    command! -nargs=0 OrganizeImports :call CocAction('runCommand', 'editor.action.organizeImport')
    augroup coc-go-commands
      autocmd!
      autocmd FileType go call coc#config('coc.preferences', {'messageLevel': 'error',})
      autocmd BufWritePre *.go :OrganizeImports
    augroup END
  " }}}
" }}}
" # vim:foldmethod=marker
