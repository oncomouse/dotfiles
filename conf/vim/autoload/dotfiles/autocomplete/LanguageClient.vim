function! dotfiles#autocomplete#LanguageClient#init() abort
  if g:complete_package ==# 'clap'
    " Use Clap for selection in LSP:
    function! MySelectionUI(source, sink) abort
      return clap#run({'id': 'LCN', 'source': a:source, 'sink': a:sink})
    endfunction
    let g:LanguageClient_selectionUI = function('MySelectionUI')
  endif
  " Documentation Function
  function! s:show_documentation() abort
    if (index(['vim','help'], &filetype) >= 0)
      execute 'h '.expand('<cword>')
    else
      call LanguageClient#textDocument_hover()
    endif
  endfunction
  " Standard commands (bindings are in vimrc)
  nmap <Plug>(dotfiles-rename) <Plug>(lcn-rename)
  nmap <Plug>(dotfiles-commands) <Plug>(lcn-menu)
  nmap <Plug>(dotfiles-definition) <Plug>(lcn-definition)
  nmap <Plug>(dotfiles-type-definition) <Plug>(lcn-type-definition)
  nmap <Plug>(dotfiles-implementation) <Plug>(lcn-implementation)
  nmap <Plug>(dotfiles-references) <Plug>(lcn-references)
  nmap <Plug>(dotfiles-documentation) :<C-u>call <SID>show_documentation()<CR>
  nmap <Plug>(dotfiles-codelens) <Plug>(lcn-code-lens-action)
  nmap <Plug>(dotfiles-codeaction) <Plug>(lcn-code-action)
  vmap <Plug>(dotfiles-codeaction-selected) <Plug>(lcn-code-action)
  command! Symbols call LanguageClient#textDocument_documentSymbol()

endfunction
