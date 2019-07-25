function! dotfiles#lsp#mappings() abort
  " Remap keys for gotos
  nmap <silent> gd :call LanguageClient_textDocument_definition()<CR>:normal! m`<CR>
  nmap <silent> gy :call LanguageClient_textDocument_typeDefinition()<CR>
  nmap <silent> gi :call LanguageClient_textDocument_implementation()<CR>
  nmap <silent> gr :call LanguageClient_textDocument_references()<CR>
  " Use U to show documentation in preview window
  nmap <silent> U :call LanguageClient#textDocument_hover()<CR>
  " Remap for rename current word
  nmap <leader>rn :call LanguageClient#textDocument_rename()<CR>
endfunction

