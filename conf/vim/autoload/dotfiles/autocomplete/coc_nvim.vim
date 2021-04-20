function! s:has_floating_window() abort
  " MenuPopupChanged was renamed to CompleteChanged -> https://github.com/neovim/neovim/pull/9819
  return (exists('##MenuPopupChanged') || exists('##CompleteChanged')) && exists('*nvim_open_win')
endfunction

function! dotfiles#autocomplete#coc_nvim#init() abort
  nmap <Plug>(dotfiles-diagnostic-next) <Plug>(coc-diagnostic-next)
  nmap <Plug>(dotfiles-diagnostic-previous) <Plug>(coc-diagnostic-previous)
  call dotfiles#autocomplete#coc_nvim#configuration()
  call dotfiles#autocomplete#coc_nvim#keyboard()
  call dotfiles#autocomplete#coc_nvim#fuzzy()
  call dotfiles#autocomplete#coc_nvim#syntax()
  call dotfiles#autocomplete#coc_nvim#writing()
endfunction

function! dotfiles#autocomplete#coc_nvim#syntax() abort
  augroup jsonc-syntax-coc
    autocmd!
    autocmd FileType json syntax match Comment +\/\/.\+$+
  augroup END
endfunction

function! dotfiles#autocomplete#coc_nvim#configuration() abort
  call coc#config('coc.preferences', {
      \ 'hoverTarget': s:has_floating_window() ? 'float' : 'echo',
      \ })
  call coc#config('suggest', {
      \ 'echodocSupport': 1,
      \ 'floatEnable': s:has_floating_window(),
      \ })
  call coc#config('signature', {
      \ 'target': s:has_floating_window() ? 'float' : 'echo',
      \ })
  call coc#config('diagnostics', {
      \ 'messageTarget': s:has_floating_window() ? 'float' : 'echo',
      \ })
endfunction

function! dotfiles#autocomplete#coc_nvim#keyboard() abort
  function! s:show_documentation() abort
    if (index(['vim','help'], &filetype) >= 0)
      execute 'h '.expand('<cword>')
    else
      call CocAction('doHover')
    endif
  endfunction
  vmap <Plug>(dotfiles-codeaction-selected) <Plug>(coc-codeaction-selected)
  nmap <Plug>(dotfiles-codeaction) <Plug>(coc-codeaction-line)
  nmap <Plug>(dotfiles-rename) <Plug>(coc-rename)
  nmap <Plug>(dotfiles-definition) <Plug>(coc-definition)
  nmap <Plug>(dotfiles-type-definition) <Plug>(coc-type-definition)
  nmap <Plug>(dotfiles-implementation) <Plug>(coc-implementation)
  nmap <Plug>(dotfiles-references) <Plug>(coc-references)
  nmap <Plug>(dotfiles-documentation) :<C-u>call <SID>show_documentation()<CR>
  nmap <Plug>(dotfiles-document-symbols) :<C-u>CocList -I symbols<CR>
  nmap <Plug>(dotfiles-commands) :<C-u>CocList commands<CR>
  nmap <Plug>(dotfiles-codelens) <Plug>(coc-codelens-action)
  imap <expr><TAB> pumvisible() ? "\<C-n>" : "<TAB>"
  imap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<S-TAB>"
  " append result on current expression
  nmap <Leader>ca <Plug>(coc-calc-result-append)
  " replace result on current expression
  nmap <Leader>cr <Plug>(coc-calc-result-replace)
endfunction

function! dotfiles#autocomplete#coc_nvim#fuzzy() abort
  " (Implement fzf.vim lists for CocList)
  let s:is_win = has('win32') || has('win64')
  function! s:shortpath() abort
    let short = fnamemodify(getcwd(), ':~:.')
    if !has('win32unix')
      let short = pathshorten(short)
    endif
    let slash = (s:is_win && !&shellslash) ? '\' : '/'
    return empty(short) ? '~'.slash : short . (short =~ escape(slash, '\').'$' ? '' : slash)
  endfunction

  function! CocFiles(dir, ...) abort
    let args = {}
    if !empty(a:dir)
      if !isdirectory(expand(a:dir))
        echohl WarningMsg
        echom 'Invalid directory'
        echohl None
      endif
      let slash = (s:is_win && !&shellslash) ? '\\' : '/'
      let dir = substitute(a:dir, '[/\\]*$', slash, '')
      let args.dir = dir
      exe 'CocList files '.expand(dir)
    else
      exe 'CocList files'
    endif
  endfunction
  nmap <Plug>(dotfiles-files) :<C-u>CocList files<CR>
  nmap <Plug>(dotfiles-home-files) :<C-u>call CocFiles('~')<CR>
  nmap <Plug>(dotfiles-buffers) :<C-u>CocList buffers<CR>
  nmap <Plug>(dotfiles-windows) :<C-u>CocList windows<CR>
  nmap <Plug>(dotfiles-lines) :<C-u>CocList lines<CR>
  nmap <Plug>(dotfiles-commands) :<C-u>CocList cmdhistory<CR>
endfunction

function! dotfiles#autocomplete#coc_nvim#writing() abort
  augroup coc-bibtex
    autocmd!
    autocmd FileType markdown nnoremap <silent> <C-C> :execute 'CocList bibtex'<CR>
    autocmd FileType markdown inoremap <silent> <C-C> <c-g>u<c-o>:execute 'CocList bibtex'<CR>
  augroup END
  augroup coc-markdown
    autocmd!
    autocmd FileType markdown call coc#config('coc.source.buffer.enable', 0)
    autocmd FileType markdown call coc#config('coc.source.around.enable', 0)
    autocmd FileType markdown call coc#config('coc.source.snippets.enable', 0)
    autocmd FileType markdown call coc#config('coc.source.file.enable', 0)
    autocmd FileType markdown call coc#config('coc.source.tmux.enable', 0)
  augroup END
  function! CocBufferOn() abort
    call coc#config('coc.source.buffer.enable', 1)
    call coc#config('coc.source.around.enable', 1)
  endfunction
  function! CocBufferOff() abort
    call coc#config('coc.source.buffer.enable', 0)
    call coc#config('coc.source.around.enable', 0)
  endfunction
  command! CocBufferOn call CocBufferOn()
  command! CocBufferOff call CocBufferOff()
endfunction
