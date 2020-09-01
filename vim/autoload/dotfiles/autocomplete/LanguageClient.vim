function! dotfiles#autocomplete#LanguageClient#init() abort
  " let g:LanguageClient_loggingFile = expand('~/LanguageClient_log.txt')
  let g:LanguageClient_settingsPath = expand('~/dotfiles/vim/lsp/settings.json')
  " Turn off all diagnostic stuff (pump it all to ALE):
  let g:LanguageClient_diagnosticsEnable = v:false
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

  " Only load LSP commands if we are in a buffer where they exist:
  function! LC_maps() abort
    if has_key(g:LanguageClient_serverCommands, &filetype)
      nmap <silent> <F2> <Plug>(lcn-rename)
      nmap <F5> <Plug>(lcn-menu)
      nmap <silent> gd <Plug>(lcn-definition)
      nmap <silent> gy <Plug>(lcn-type-definition)
      nmap <silent> gi <Plug>(lcn-implementation)
      nmap <silent> gr <Plug>(lnc-references)
      nmap <silent> K :<C-u>call <SID>show_documentation()<CR>
      command! Symbols call LanguageClient#textDocument_documentSymbol()
    endif
  endfunction
  augroup lc_maps
    autocmd!
    autocmd FileType * call LC_maps()
  augroup END
  " Turn off diagnostics in solargraph and just run rubocop through ALE:
  let g:LanguageClient_serverCommands = {
        \ 'javascript': ['/usr/local/bin/typescript-language-server', '--stdio'],
        \ 'javascriptreact': ['/usr/local/bin/typescript-language-server', '--stdio'],
        \ 'typescript': ['/usr/local/bin/typescript-language-server', '--stdio'],
        \ 'typescriptreact': ['/usr/local/bin/typescript-language-server', '--stdio'],
        \ 'python': ['/usr/local/bin/jedi-language-server'],
        \ 'ruby': ['~/.asdf/shims/solargraph', 'stdio'],
        \ 'html': ['/usr/local/bin/html-languageserver', '--stdio'],
        \ 'scss': ['/usr/local/bin/css-languageserver', '--stdio'],
        \ 'css': ['/usr/local/bin/css-languageserver', '--stdio'],
        \ 'json': ['/usr/local/bin/json-languageserver', '--stdio'],
        \ 'lua': ['/usr/bin/java', '-cp', '~/dotfiles/lsp/EmmyLua-LS-all.jar', 'com.tang.vscode.MainKt'],
        \ 'vim': ['/usr/local/bin/vim-language-server', '--stdio'],
        \}
endfunction
