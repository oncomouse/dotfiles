nnoremap <silent><buffer><expr> <CR>
      \ denite#do_map('do_action')
nnoremap <silent><buffer><expr> d
      \ denite#do_map('do_action', 'delete')
nnoremap <silent><buffer><expr> p
      \ denite#do_map('do_action', 'preview')
nnoremap <silent><buffer><expr> <C-v>
      \ denite#do_map('do_action', 'vsplit')
nnoremap <silent><buffer><expr> <C-x>
      \ denite#do_map('do_action', 'split')
nnoremap <silent><buffer><expr> <Esc>
      \ denite#do_map('quit')
nnoremap <silent><buffer><expr> i
      \ denite#do_map('open_filter_buffer')
nnoremap <silent><buffer><expr> <Space>
      \ denite#do_map('toggle_select').'j'
