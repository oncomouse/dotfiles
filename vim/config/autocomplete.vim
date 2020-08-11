scriptencoding utf-8
set completeopt-=preview
" float-preview.nvim{{{
  let g:float_preview#docked = 0
" }}}
" ALE {{{
  let g:ale_javascript_standard_executable = 'semistandard'
  command! Format ALEFix
  let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
  "
  " Jump between ALE Errors:
  nmap <silent> <leader>d :<C-u>Clap loclist<CR>
  nmap <silent> [d :<C-u>ALEPreviousWrap<CR>
  nmap <silent> ]d :<C-u>ALENextWrap<CR>
  "
  let g:ale_lint_on_insert_leave = 1
  let g:ale_cursor_detail = 0
  let g:ale_disable_lsp = 1
  let g:ale_fix_on_save = 1
  let g:ale_pattern_options = {
    \  '\.min.js$': {'ale_enabled': 0},
    \  'build/.*$': {'ale_enabled': 0},
    \}
" }}}
" vim-clap {{{
" Old FZF Interface:
command!      -bang -nargs=? -complete=dir FZF    exe 'Clap files ++query='.<q-args>
command! Buffers :exe 'Clap buffers'
command! Windows :exe 'Clap windows'
command! BLines :exe 'Clap lines'
command! History :exe 'Clap command_history'
nnoremap <silent> <leader>y  :<C-u>Clap yanks<CR>
" Implement Ag
command! -nargs=+ -complete=custom,s:GrepArgs Ag exe 'Clap grep2 ++query='.<q-args>
vnoremap <silent> <leader>/ :<C-u>Clap grep2 ++query=@visual<CR>
" }}}
" Deoplete {{{
let g:deoplete#enable_at_startup = 1
" }}}
" vim-lsp-settings {{{
  function! s:show_documentation()
    if (index(['vim','help'], &filetype) >= 0)
      execute 'h '.expand('<cword>')
    else
      LspHover
    endif
  endfunction
  " Turn off all diagnostic stuff (pump it all to ALE):
  let g:lsp_signs_enabled = 0
  let g:lsp_diagnostics_echo_cursor = 0
  let g:lsp_highlights_enabled = 0
  let g:lsp_textprop_enabled = 0
  let g:lsp_diagnostics_float_cursor = 0
  let g:lsp_virtual_text_enabled = 0
  let g:lsp_preview_float = 0
  nmap <silent> gd :<C-u>LspPeekDefinition<CR>
  nmap <silent> gy :<C-u>LspPeekTypeDefinition<CR>
  nmap <silent> gi :<C-u>LspPeekImplementation<CR>
  nmap <silent> gr :<C-u>LspReferences<CR>
  nmap <silent> K :<C-u>call <SID>show_documentation()<CR>
  " Turn off diagnostics in solargraph and just run rubocop through ALE:
  let g:lsp_settings = {
        \  'solargraph': {
        \    'config': {
        \      'diagnostics': v:false,
        \    },
        \  },
        \}
" }}}
" # vim:foldmethod=marker
