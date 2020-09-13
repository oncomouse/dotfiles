function! dotfiles#autocomplete#LanguageClient#init() abort
  " let g:LanguageClient_loggingFile = expand('~/LanguageClient_log.txt')
  let g:LanguageClient_settingsPath = expand('~/dotfiles/vim/lsp/settings.json')
  " Turn off all diagnostic stuff (pump it all to ALE):
  let g:LanguageClient_diagnosticsEnable = v:false
  " Debug:
  " let g:LanguageClient_loggingFile = expand('~/lc.log')
  " let g:LanguageClient_loggingLevel = 'DEBUG'
  " Always use hover:
  let g:LanguageClient_hoverPreview = 'Always'
  " Documentation Function
  function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
      execute 'h '.expand('<cword>')
    else
      call LanguageClient#textDocument_hover()
    endif
  endfunction

  if g:complete_package ==# 'clap'
    " Use Clap for selection in LSP:
    function! MySelectionUI(source, sink) abort
      return clap#run({'id': 'LCN', 'source': a:source, 'sink': a:sink})
    endfunction
    let g:LanguageClient_selectionUI = function('MySelectionUI')
  endif
  " Standard commands (bindings are in vimrc)
  nmap <Plug>(dotfiles-rename) <Plug>(lcn-rename)
  nmap <Plug>(dotfiles-commands) <Plug>(lcn-menu)
  nmap <Plug>(dotfiles-definition) <Plug>(lcn-definition)
  nmap <Plug>(dotfiles-type-definition) <Plug>(lcn-type-definition)
  nmap <Plug>(dotfiles-implementation) <Plug>(lcn-implementation)
  nmap <Plug>(dotfiles-references) <Plug>(lcn-references)
  nmap <Plug>(dotfiles-documentation) :<C-u>call <SID>show_documentation()<CR>
  command! Symbols call LanguageClient#textDocument_documentSymbol()

  let g:LanguageClient_serverCommands = {
        \ 'python': ['/usr/local/bin/jedi-language-server'],
        \ 'ruby': ['~/.asdf/shims/solargraph', 'stdio'],
        \ 'html': ['/usr/local/bin/html-languageserver', '--stdio'],
        \ 'scss': ['/usr/local/bin/css-languageserver', '--stdio'],
        \ 'css': ['/usr/local/bin/css-languageserver', '--stdio'],
        \ 'json': ['/usr/local/bin/json-languageserver', '--stdio'],
        \ 'lua': ['/usr/bin/java', '-cp', '~/dotfiles/lsp/EmmyLua-LS-all.jar', 'com.tang.vscode.MainKt'],
        \ 'vim': ['/usr/local/bin/vim-language-server', '--stdio'],
        \ 'markdown': ['~/.local/bin/citation-langserver'],
        \}
endfunction
