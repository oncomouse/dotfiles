scriptencoding utf-8
" Coc Extensions {{{
  " Once your pull request gets accepted, add back coc-go
  let g:coc_global_extensions = [
  \   'coc-bibtex',
  \   'coc-calc',
  \   'coc-css',
  \   'coc-fish',
  \   'coc-go',
  \   'coc-html',
  \   'coc-json',
  \   'coc-lists',
  \   'coc-prettier',
  \   'coc-python',
  \   'coc-solargraph',
  \   'coc-styled-components',
  \   'coc-tsserver',
  \   'coc-vimlsp',
  \   'coc-yaml',
  \   'coc-yank',
  \]
" }}}
" Coc Configuration {{{
  " " Coc Floating Window Support:
  " call coc#config('coc.preferences', {
  "     \ 'hoverTarget': dotfiles#has_floating_window() ? 'float' : 'echo',
  "     \ })
  " call coc#config('suggest', {
  "     \ 'echodocSupport': 1,
  "     \ 'floatEnable': dotfiles#has_floating_window(),
  "     \ })
  " call coc#config('signature', {
  "     \ 'target': dotfiles#has_floating_window() ? 'float' : 'echo',
  "     \ })
  " call coc#config('diagnostics', {
  "     \ 'messageTarget': dotfiles#has_floating_window() ? 'float' : 'echo',
  "     \ })
" }}}
" Coc Keyboard shortcuts: {{{
  " function! s:show_documentation()
  "   if (index(['vim','help'], &filetype) >= 0)
  "     execute 'h '.expand('<cword>')
  "   else
  "     call CocAction('doHover')
  "   endif
  " endfunction
  " imap <expr><TAB> pumvisible() ? "\<C-n>" : dotfiles#smart_tab()
  " imap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<S-TAB>"
  " nmap <silent> gd <Plug>(coc-definition)
  " nmap <silent> gy <Plug>(coc-type-definition)
  " nmap <silent> gi <Plug>(coc-implementation)
  " nmap <silent> gr <Plug>(coc-references)
  " nmap <silent> ]d <Plug>(coc-diagnostic-next)
  " nmap <silent> [d <Plug>(coc-diagnostic-prev)
  " nmap <silent> []d :<C-u>CocList diagnostics<CR>
  " nmap <silent> <leader>d :<C-u>CocList diagnostics<CR>
  " nnoremap <silent> K :call <SID>show_documentation()<CR>
  " nmap <leader>rn <Plug>(coc-rename)
  " command! Symbols :<C-u>CocList -I symbols<cr>
  " nmap <leader>s :Symbols<CR>
  " " append result on current expression
  " nmap <Leader>ca <Plug>(coc-calc-result-append)
  " " replace result on current expression
  " nmap <Leader>cr <Plug>(coc-calc-result-replace)
" }}}
" Coc Formatting {{{
  " set formatexpr=CocAction('formatSelected')
  " xmap <leader>f  <Plug>(coc-format-selected)
  " nmap <leader>f  <Plug>(coc-format-selected)
  " command! -nargs=0 Format :call CocAction('format')
" }}}
" Coc Fuzzy {{{
  " (Implement fzf.vim lists for CocList)
  " let s:is_win = has('win32') || has('win64')
  " function! s:shortpath()
  "   let short = fnamemodify(getcwd(), ':~:.')
  "   if !has('win32unix')
  "     let short = pathshorten(short)
  "   endif
  "   let slash = (s:is_win && !&shellslash) ? '\' : '/'
  "   return empty(short) ? '~'.slash : short . (short =~ escape(slash, '\').'$' ? '' : slash)
  " endfunction

  " function! CocFiles(dir, ...)
  "   let args = {}
  "   if !empty(a:dir)
  "     if !isdirectory(expand(a:dir))
  "       echohl WarningMsg
  "       echom 'Invalid directory'
  "       echohl None
  "     endif
  "     let slash = (s:is_win && !&shellslash) ? '\\' : '/'
  "     let dir = substitute(a:dir, '[/\\]*$', slash, '')
  "     let args.dir = dir
  "     exe 'CocList files '.expand(dir)
  "   else
  "     exe 'CocList files'
  "   endif
  " endfunction
  " command!      -bang -nargs=? -complete=dir FZF       call CocFiles(<q-args>, <bang>0)
  " command! Buffers :exe 'CocList buffers'
  " command! Windows :exe 'CocList windows'
  " command! BLines :exe 'CocList lines'
  " command! History :exe 'CocList cmdhistory'
  " nnoremap <silent> <leader>y  :<C-u>CocList -A --normal yank<cr>
  " " Implement Ag
  " command! -nargs=+ -complete=custom,s:GrepArgs Ag exe 'CocList grep '.<q-args>

" }}}
" Coc Filetypes {{{
  " Go {{{
    " command! -nargs=0 OrganizeImports :call CocAction('runCommand', 'editor.action.organizeImport')
    " augroup coc-go-commands
    "   autocmd!
    "   autocmd FileType go call coc#config('coc.preferences', {'messageLevel': 'error',})
    "   autocmd BufWritePre *.go :OrganizeImports
    " augroup END
  " }}}
  " HTML {{{
    " augroup coc-html-commands
    "   autocmd FileType html let b:coc_pairs_disabled = ['<']
    " augroup END
  " }}}
" }}}
" ALE {{{
  let g:ale_set_loclist = 0
  let g:ale_set_quickfix = 1
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
" # vim:foldmethod=marker
