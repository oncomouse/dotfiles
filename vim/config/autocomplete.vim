" Autocomplete:
" COC {{
  " Add extensions
  let coc_extensions = [
  \   'coc-omni',
  \   'coc-neosnippet',
  \   'coc-tsserver',
  \   'coc-json',
  \   'coc-html',
  \   'coc-css',
  \   'coc-python',
  \   'coc-gocode',
  \]

  function! s:load_extension(ext) abort
    if !isdirectory(expand('~/.config/coc/extensions/node_modules/'.a:ext))
      call coc#add_extension(a:ext)
    endif
  endfunction
  for ext in coc_extensions
    call <SID>load_extension(ext)
  endfor
  " call coc#config('tsserver', {
  "     \  'log': 'verbose',
  "     \  'trace.server': 'verbose',
  "     \ })
  if !empty(glob('/usr/local/bin/reason-language-server'))
    call coc#config('languageserver', {
      \  'reason': {
      \    'command': '/usr/local/bin/reason-language-server',
      \    'filetypes': ['reason'],
      \    'trace.server': 'verbose',
      \    'rootPatterns': ['bsconfig.json', 'package.json', '.git/', '.merlin'],
      \    'settings': {'reason_language_server' : {'format_width': 120}},
      \  }
      \})
  endif
  call coc#config('suggest.snippetIndicator', '►')
  " Configure omnifunc completion for Pandoc:
  call coc#config('coc.source.omni.filetypes', [
      \   'pandoc'
      \])
  " JavaScript Config:
  " Don't do typechecking for JavaScript:
  call coc#config('javascript.validate.enable', 0)
  " Format JavaScript the way I like:
  call coc#config('javascript.format', {
      \   'insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces': 0,
      \   'insertSpaceBeforeFunctionParenthesis': 0,
      \})
  " CoC Formatting:
  vmap <silent> <leader>f <Plug>(coc-format-selected)<CR>
  nmap <silent> <leader>f <Plug>(coc-format-selected)<CR>
  command! -nargs=0 Format :call CocAction('format')
  " call coc#config('suggest.noselect', 0)
  function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~ '\s'
  endfunction

  inoremap <silent><expr> <S-Tab>
        \ pumvisible() ? "\<C-p>" :
        \ <SID>check_back_space() ? "\<S-Tab>" :
        \ coc#refresh()
  inoremap <silent><expr> <TAB>
  \ pumvisible() ? coc#_select_confirm() :
  \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
  \ <SID>check_back_space() ? "\<TAB>" :
  \ coc#refresh()
" }}
