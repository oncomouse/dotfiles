function! dotfiles#autocomplete#fzf#init()
  command! -bang -nargs=? -complete=dir Files
    \ call fzf#vim#files(<q-args>, fzf#vim#with_preview({'options': ['--reverse', '--info=inline']}), <bang>0)
  if g:dotfiles_mode ==# 'desktop'
    " command! Yanks exe 'FZFNeoyank'
    nnoremap <leader>Y :FZFNeoyank " P<cr>
    vnoremap <leader>y :FZFNeoyankSelection<cr>
    let g:fzf_lsp_preview_window = ['right:40%', 'ctrl-/']
    " Load fzf_lsp handlers:
    lua require'fzf_lsp'.setup()
  endif
  let $FZF_DEFAULT_OPTS .= ' --reverse'
  let g:fzf_layout = { 'window': { 'width': 1, 'height': 0.4, 'yoffset': 1, 'border': 'top' } }
  " let $FZF_DEFAULT_COMMAND = 'fd --type f --hidden'
  let g:fzf_action = {
    \ 'ctrl-s': 'split',
    \ 'ctrl-v': 'vsplit',
    \ 'ctrl-t': 'tabnew',
    \ 'ctrl-e': 'edit',
    \ }
  let g:fzf_nvim_statusline = 0 " disable statusline overwriting
  " Standard dotfiles bindings:
  nmap <Plug>(dotfiles-files) :<C-u>Files<CR>
  nmap <Plug>(dotfiles-home-files) :<C-u>Files ~<CR>
  nmap <Plug>(dotfiles-buffers) :<C-u>Buffers<CR>
  nmap <Plug>(dotfiles-windows) :<C-u>Windows<CR>
  nmap <Plug>(dotfiles-lines) :<C-u>BLines<CR>
  nmap <Plug>(dotfiles-commands) :<C-u>Commands<CR>
  if g:dotfiles_mode ==# 'desktop'
    nmap <Plug>(dotfiles-yanks) :<C-u>FZFNeoyank<CR>
  endif
endfunction
