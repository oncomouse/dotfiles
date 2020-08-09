scriptencoding utf-8
" ALE {{{
  let g:ale_set_loclist = 0
  let g:ale_set_quickfix = 1
  let g:ale_javascript_standard_executable = 'semistandard'
  command! Format ALEFix
  " Better ALE Msg Format
  " let g:ale_echo_msg_error_str = 'E'
  " let g:ale_echo_msg_warning_str = 'W'
  let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
  "
  " Jump between ALE Errors:
  nmap <silent> <leader>d :<C-u>Denite quickfix<CR>
  nmap <silent> [d :<C-u>ALEPreviousWrap<CR>
  nmap <silent> ]d :<C-u>ALENextWrap<CR>
  "
  let g:ale_lint_on_insert_leave = 1
  let g:ale_cursor_detail = 0
  let g:ale_fix_on_save = 1
  let g:ale_pattern_options = {
    \  '\.min.js$': {'ale_enabled': 0},
    \  'build/.*$': {'ale_enabled': 0},
    \}
" }}}
" Denite {{{
" \ 'mode' : 'insert',
" \ 'start_filter' : 1,
" \ 'quit' : 1,
" \ 'highlight_matched_char' : 'MoreMsg',
" \ 'highlight_matched_range' : 'MoreMsg',
call denite#custom#option('default', 'winheight', 10)
call denite#custom#option('default', 'direction', 'rightbelow')
call denite#custom#option('default', 'statusline', v:false)
call denite#custom#var('file/rec', 'command', ['ag', '--follow', '--nocolor', '--nogroup', '-g', ''])
call denite#custom#option('default', 'prompt', 'λ')
call denite#custom#var('grep', 'command', ['ag'])
call denite#custom#var('grep', 'default_opts', ['-i', '--vimgrep'])
call denite#custom#var('grep', 'recursive_opts', [])
call denite#custom#var('grep', 'pattern_opt', [])
call denite#custom#var('grep', 'separator', ['--'])
call denite#custom#var('grep', 'final_opts', [])
call denite#custom#source('file_rec', 'sorters', ['sorter_sublime'])
call denite#custom#filter('matcher_ignore_globs', 'ignore_globs',
      \ [ '.git/', '.ropeproject/', '__pycache__/*', '*.pyc', 'node_modules/',
      \   'venv/', 'images/', '*.min.*', 'img/', 'fonts/', '*.png'])
command!      -bang -nargs=? -complete=dir FZF    exe 'Denite file/rec '.<q-args>
command! Buffers :exe 'Denite buffer'
command! Windows :exe 'Denite window'
command! BLines :exe 'Denite line'
command! History :exe 'Denite history'
nnoremap <silent> <leader>y  :<C-u>Denite neoyank<CR>
" Implement Ag
command! -nargs=+ -complete=custom,s:GrepArgs Ag exe 'Denite grep:::'.<q-args>
" }}}
" Deoplete {{{
let g:deoplete#enable_at_startup = 1
let g:deoplete#sources#biblatex#bibfile = g:bibliography_file
let g:deoplete#sources#biblatex#addinfo = 1
call deoplete#custom#source('biblatex', 'filetypes', ['markdown'])
" }}}
" vim-lsp-settings {{{
" Alias the format document command:
" Turn off all diagnostic stuff (pump it all to ALE):
let g:lsp_signs_enabled = 0
let g:lsp_diagnostics_echo_cursor = 0
let g:lsp_highlights_enabled = 0
let g:lsp_textprop_enabled = 0
let g:lsp_diagnostics_float_cursor = 0
let g:lsp_virtual_text_enabled = 0
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
