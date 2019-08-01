function! dotfiles#go#mappings(...) abort
  let l:on = get(a:, 0, 0)
  if l:on
    nmap  <silent> gd :GoDef<CR>
    nmap <silent> gy :GoDefType<CR>
    nmap <silent> gi :GoImplements<CR>
    nmap <silent> gr :GoReferrers<CR>
    nmap <silent> U :GoDocBrowser<CR>
    nmap <silent> <leader>rn :GoRename<CR>
    nmap <leader>rt <Plug>(go-run-tab)
    nmap <leader>rs <Plug>(go-run-split)
    nmap <leader>rv <Plug>(go-run-vertical)
  else
    call dotfiles#lsp#load()
  endif
endfunction
