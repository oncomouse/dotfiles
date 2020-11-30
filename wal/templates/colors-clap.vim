let s:save_cpo = &cpoptions
set cpoptions&vim

let s:palette = {{}}

let s:palette.display = {{ 'ctermbg': '235', 'guibg': '#{color8.strip}' }}

" Let ClapInput, ClapSpinner and ClapSearchText use the same backgound.
let s:bg0 = {{ 'ctermbg': '60', 'guibg': '#4a4a4b' }}
let s:palette.input = s:bg0
let s:palette.spinner = extend({{ 'ctermfg': '11', 'guifg':'#{color3.strip}', 'cterm': 'bold', 'gui': 'bold'}}, s:bg0)
let s:palette.search_text = extend({{ 'ctermfg': '195', 'guifg': '#{foreground.strip}', 'cterm': 'bold', 'gui': 'bold' }}, s:bg0)

let s:palette.preview = {{ 'ctermbg': '0', 'guibg': '#{background.strip}' }}

let s:palette.selected = {{ 'ctermfg': '81', 'guifg': '#{color14.strip}', 'cterm': 'bold,underline', 'gui': 'bold,underline' }}
let s:palette.current_selection = {{ 'ctermbg': '236', 'guibg': '#4a4a4b', 'cterm': 'bold', 'gui': 'bold' }}

let s:palette.selected_sign = {{ 'ctermfg': '196', 'guifg': '#{color13.strip}' }}
let s:palette.current_selection_sign = s:palette.selected_sign
let g:clap#themes#wal#palette = s:palette

let g:clap_fuzzy_match_hl_groups = [
      \ [22, '#{color4.strip}'],
      \ [23, '#6f9dab'],
      \ [24, '#709ea7'],
      \ [25, '#719fa2'],
      \ [26, '#72a09e'],
      \ [27, '#73a19a'],
      \ [58, '#74a295'],
      \ [59, '#75a391'],
      \ [60, '#76a48d'],
      \ [61, '#77a588'],
      \ [62, '#78a684'],
      \ [63, '#{color14.strip}'],
      \]
let &cpoptions = s:save_cpo
unlet s:save_cpo


