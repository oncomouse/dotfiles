" Loaded?
let g:dotfiles#lsp#loaded = 0
" Key Maps
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction
function! dotfiles#lsp#mappings() abort
  if !dotfiles#lsp_test() || g:dotfiles#lsp#loaded == 1
    return
  endif
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
endfunction
" Formatting:
function! dotfiles#lsp#formatting_commands() abort
  if !dotfiles#lsp_test() || g:dotfiles#lsp#loaded == 1
    return
  endif
  " Configure omnifunc completion for Pandoc:
  call coc#config('coc.source.omni.filetypes', [
      \   'pandoc',
      \   'go',
      \])
  " JavaScript Config:
  " Don't do typechecking for JavaScript:
  call coc#config('javascript.validate.enable', 0)
  " Format JavaScript the way I like:
  call coc#config('javascript.format', {
      \   'insertSpaceAfterOpeningAndBeforeClosingNonemptyBraces': 0,
      \   'insertSpaceBeforeFunctionParenthesis': 0,
      \})
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
endfunction
function! dotfiles#lsp#load() abort
  call dotfiles#lsp#formatting_commands()
  call dotfiles#lsp#mappings()
  let g:dotfiles#lsp#loaded = 1
endfunction

let g:dotfiles#lsp#started = {}
function! dotfiles#lsp#start_lsp(server_executable, ...) abort
  if has_key(g:dotfiles#lsp#started, a:server_executable)
    return
  endif
  let g:dotfiles#lsp#started[a:server_executable] = 1
  call jobstart ( a:server_executable, { 'detach' : 1 } )
endfunction
