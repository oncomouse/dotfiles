" Coc Extensions {{{
  " Once your pull request gets accepted, add back coc-go
  let g:coc_global_extensions = [
  \   'coc-css',
  \   'coc-fish',
  \   'coc-html',
  \   'coc-json',
  \   'coc-lists',
  \   'coc-omni',
  \   'coc-pairs',
  \   'coc-python',
  \   'coc-solargraph',
  \   'coc-tsserver',
  \   'coc-vimlsp',
  \   'coc-yank',
  \]
" }}}
" Coc Configuration {{{
  " call coc#config('tsserver', {
  "     \  'log': 'verbose',
  "     \  'trace.server': 'verbose',
  "     \ })
  " Turn of vim-lsp diagnostics:
  call coc#config('vimlsp.diagnostic.enable', 0)
  " Load errors in ALE:
  call coc#config('coc.preferences.diagnostic.displayByAle', 1)
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
" }}}
" Coc Keyboard shortcuts: {{{
  imap <expr><TAB> pumvisible() ? "\<C-n>" : dotfiles#smart_tab()
  imap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<S-TAB>"
  " Remap keys for gotos
  nmap <silent> gd <Plug>(coc-definition)
  nmap <silent> gy <Plug>(coc-type-definition)
  nmap <silent> gi <Plug>(coc-implementation)
  nmap <silent> gr <Plug>(coc-references)
  " Use U to show documentation in preview window
  nnoremap <silent> K :call <SID>show_documentation()<CR>

  " Remap for rename current word
  nmap <leader>rn <Plug>(coc-rename)
  " Show symbols:
  command! Symbols :<C-u>CocList -I symbols<cr>
  nmap <leader>s :Symbols<CR>
  " Formatting
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
