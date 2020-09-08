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
  " Standard commands (bindings are in vimrc)
  command! Rename call LanguageClient#textDocument_rename()
  command! Definition call LanguageClient#textDocument_definition()
  command! TypeDefinition call LanguageClient#textDocument_typeDefinition()
  command! Implementation call LanguageClient#textDocument_implementation()
  command! References call LanguageClient#textDocument_references()
  command! Documentation call <SID>show_documentation()<CR>
  command! Commands call LanguageClient_contextMenu()

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
