function! dotfiles#floating_scratch#scratch() abort
  let buf = nvim_create_buf(v:false, v:true)
  let width = float2nr(&columns - (&columns * 2 / 10))
  let height = &previewheight
  let y = float2nr((&lines - height) / 2)
  let x = float2nr((&columns - width) / 2)
  let opts = {'relative': 'win', 'width': width, 'height': height, 'col': x,
      \ 'row': y, 'anchor': 'NW', 'style': 'minimal'}
  let win = nvim_open_win(buf, 1, opts)
endfunction
