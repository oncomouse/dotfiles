" Load cfilter in quickfix view:
packadd cfilter
" Turn off FZF trigger for easier QF navigation:
nmap <buffer> <C-P> <C-P>

" Open in a split:
nnoremap <silent><buffer> <C-s> <cmd>lua require("qf_helper").open_split("split")<CR>
" Open in a vertical split:
nnoremap <silent><buffer> <C-v> <cmd>lua require("qf_helper").open_split("vsplit")<CR>
" Open without leaving quickfix:
nnoremap <silent><buffer> <C-y> <CR><C-W>p
nnoremap <silent><buffer> <C-j> j<CR><C-W>p
nnoremap <silent><buffer> <C-k> k<CR><C-W>p
nnoremap <silent><buffer> { <cmd>lua require("qf_helper").navigate(-1, { by_file = true })<CR><C-w>p
nnoremap <silent><buffer> } <cmd>lua require("qf_helper").navigate(1, { by_file = true })<CR><C-w>p
