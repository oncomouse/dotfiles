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
      \   'insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces': 1,
      \})
  " CoC Formatting:
  vmap <S-f> <Plug>(coc-format-selected)<CR>
  nmap <leader>f <Plug>(coc-format-selected)
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
