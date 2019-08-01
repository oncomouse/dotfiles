" Loaded?
let g:dotfiles#lsp#loaded = 0
" Key Maps
function! dotfiles#lsp#mappings() abort
  if !dotfiles#lsp_test() || g:dotfiles#lsp#loaded == 1
    return
  endif
  " Remap keys for gotos
  nmap <silent> gd :call LanguageClient_textDocument_definition()<CR>:normal! m`<CR>
  nmap <silent> gy :call LanguageClient_textDocument_typeDefinition()<CR>
  nmap <silent> gi :call LanguageClient_textDocument_implementation()<CR>
  nmap <silent> gr :call LanguageClient_textDocument_references()<CR>
  " Use U to show documentation in preview window
  nmap <silent> U :call LanguageClient#textDocument_hover()<CR>
  " Remap for rename current word
  nmap <leader>rn :call LanguageClient#textDocument_rename()<CR>
  " Show symbols:
  command! Symbols call LanguageClient_textDocument_documentSymbol()
  nmap <leader>s :Symbols<CR>
endfunction
" Formatting:
function! dotfiles#lsp#formatting_commands() abort
  if !dotfiles#lsp_test() || g:dotfiles#lsp#loaded == 1
    return
  endif
  vmap <silent> <leader>f :call LanguageClient#textDocument_rangeFormatting()<CR>
  nmap <silent> <leader>f :call LanguageClient#textDocument_formatting()<CR>
  command! -nargs=0 Format :call LanguageClient#textDocument_formatting()
  " Autoformat on save:
  set formatexpr=LanguageClient#textDocument_rangeFormatting_sync()
  augroup lsp-formatting
    autocmd!
    " LSP Format on save:
    autocmd BufWritePre <buffer> if dotfiles#lsp_test() | :call LanguageClient#textDocument_formatting_sync() | endif
  augroup END
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
