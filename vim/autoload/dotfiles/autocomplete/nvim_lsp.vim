function! dotfiles#autocomplete#nvim_lsp#init()
  " Key bindings
  function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
      execute 'h '.expand('<cword>')
    else
      execute 'lua vim.lsp.buf.hover()'
    endif
  endfunction
  nnoremap <silent><Plug>(dotfiles-rename) :<C-u>lua vim.lsp.buf.rename()<CR>
  nnoremap <silent><Plug>(dotfiles-definition) :<C-u>lua vim.lsp.buf.definition()<CR>
  nnoremap <silent><Plug>(dotfiles-type-definition) :<C-u>lua vim.lsp.buf.type_definition()<CR>
  nnoremap <silent><Plug>(dotfiles-implementation) :<C-u>lua vim.lsp.buf.implementation()<CR>
  nnoremap <silent><Plug>(dotfiles-references) :<C-u>lua vim.lsp.buf.references()<CR>
  nnoremap <silent><Plug>(dotfiles-documentation) :<C-u>call <SID>show_documentation()<CR>
  nnoremap <silent><Plug>(dotfiles-commands) :<CR>
  lua require('dotfiles/nvim_lsp') 
endfunction
