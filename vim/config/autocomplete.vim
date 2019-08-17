" Coc Extensions {{{
  " Once your pull request gets accepted, add back coc-go
  let g:coc_global_extensions = [
  \   'coc-css',
  \   'coc-diagnostic',
  \   'coc-eslint',
  \   'coc-fish',
  \   'coc-html',
  \   'coc-json',
  \   'coc-lists',
  \   'coc-omni',
  \   'coc-pairs',
  \   'coc-python',
  \   'coc-solargraph',
  \   'coc-stylelint',
  \   'coc-tsserver',
  \   'coc-vimlsp',
  \   'coc-yaml',
  \   'coc-yank',
  \]
" }}}
" Coc Configuration {{{
  " call coc#config('tsserver', {
  "     \  'log': 'verbose',
  "     \  'trace.server': 'verbose',
  "     \ })
  " Turn of vim-lsp diagnostics:
  call coc#config('diagnostic-languageserver.trace.server', 'verbose')
  call coc#config('vimlsp.diagnostic.enable', 1)
  " Ruby:
  call coc#config('solargraph.diagnostics', 1)
  call coc#config('solargraph.formatting', 1)
  " Diagnostic LSP:
  call coc#config('diagnostic-languageserver', {
  \  'filetypes': {
  \    'sh': ['shellcheck'],
  \    'pandoc': ['vale'],
  \    'yaml': ['yamllint'],
  \  },
  \  'linters': {
  \    'vale': {
  \      'command': 'vale',
  \      'rootPatterns': [],
  \      'isStdout': 1,
  \      'isStderr': 0,
  \      'debounce': 100,
  \      'args': ['--output', 'JSON'],
  \      'offsetLine': 0,
  \      'offsetColumn': 0,
  \      'sourceName': 'vale',
  \      'formatLines': 14,
  \      'formatPattern': [
  \        '^\s*\{[\w\s\n\W]+?"Line": (\d+)[\w\s\n\W]+?"Message": "([^"]+)"[\w\s\n\W]+?"Severity":\s*"([^"]+)"[\w\s\n\W]+?"Span": \[\n\s+([0-9]+)[\w\s\n\W]+?\},{0,1}',
  \        {
  \          'line': 1,
  \          'column': 4,
  \          'message': [2],
  \          'security': 3,
  \        }
  \      ],
  \      'securities': {
  \        'error': 'error',
  \        'warning': 'warning',
  \        'note': 'info',
  \      },
  \    },
  \    'yamllint': {
  \      'command': 'yamllint',
  \      'rootPatterns': [],
  \      'isStdout': 1,
  \      'isStderr': 0,
  \      'debounce': 100,
  \      'args': ['-f','parsable','-'],
  \      'offsetLine': 0,
  \      'offsetColumn': 0,
  \      'sourceName': 'yamllint',
  \      'formatLines': 1,
  \      'formatPattern': [
  \        '^[^:]+:(\d+):(\d+):\s*\[([A-Za-z]+)\]\s*(.*)$',
  \        {
  \          'line': 1,
  \          'column': 2,
  \          'message': [4],
  \          'security': 3,
  \        }
  \      ],
  \      'securities': {
  \        'error': 'error',
  \        'warning': 'warning',
  \        'note': 'info',
  \      },
  \    },
  \  },
  \})
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
  imap <expr><TAB> pumvisible() ? "\<C-n>" : dotfiles#smart_tab()
  imap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<S-TAB>"
  nmap <silent> gd <Plug>(coc-definition)
  nmap <silent> gy <Plug>(coc-type-definition)
  nmap <silent> gi <Plug>(coc-implementation)
  nmap <silent> gr <Plug>(coc-references)
  nmap <silent> <leader>lk <Plug>(coc-diagnostic-prev)
  nmap <silent> <leader>lj <Plug>(coc-diagnostic-next)
  nmap <silent> <leader>ll :<C-u>CocList diagnostics<CR>
  nnoremap <silent> K :call <SID>show_documentation()<CR>
  nmap <leader>rn <Plug>(coc-rename)
  command! Symbols :<C-u>CocList -I symbols<cr>
  nmap <leader>s :Symbols<CR>
" }}}
" Coc Formatting {{{
  set formatexpr=CocAction('formatSelected')
  xmap <leader>f  <Plug>(coc-format-selected)
  nmap <leader>f  <Plug>(coc-format-selected)
  command! -nargs=0 Format :call CocAction('format')
  " Autoformat on save:
  call coc#config('coc.preferences.formatOnSaveFiletypes', [
    \ 'javascript',
    \ 'javascript.jsx',
    \ 'go',
    \ 'reason',
    \ 'python',
    \])
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
    else
      let dir = s:shortpath()
    endif
    echom dir
    exe 'CocList files '.expand(dir)
  endfunction
  command!      -bang -nargs=? -complete=dir FZF       call CocFiles(<q-args>, <bang>0)
  command! Buffers :exe 'CocList buffers'
  command! Windows :exe 'CocList windows'
  command! BLines :exe 'CocList lines'
  command! History :exe 'CocList cmdhistory'
  nnoremap <silent> <leader>rr  :<C-u>CocList -A --normal yank<cr>

" }}}
" Coc Filetypes {{{
  " JavaScript {{{
    " Format JavaScript the way I like:
    call coc#config('javascript.format', {
        \   'insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces': 0,
        \   'insertSpaceBeforeFunctionParenthesis': 0,
        \})
  " }}}
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
