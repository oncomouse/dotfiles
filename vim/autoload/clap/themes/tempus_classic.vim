let s:save_cpo = &cpoptions
set cpoptions&vim

let s:palette = {}

let s:palette.display = { 'ctermbg': '235', 'guibg': '#312e30' }

" Let ClapInput, ClapSpinner and ClapSearchText use the same backgound.
let s:bg0 = { 'ctermbg': '60', 'guibg': '#4a4a4b' }
let s:palette.input = s:bg0
let s:palette.spinner = extend({ 'ctermfg': '11', 'guifg':'#b1942b', 'cterm': 'bold', 'gui': 'bold'}, s:bg0)
let s:palette.search_text = extend({ 'ctermfg': '195', 'guifg': '#aeadaf', 'cterm': 'bold', 'gui': 'bold' }, s:bg0)

let s:palette.preview = { 'ctermbg': '0', 'guibg': '#232323' }

let s:palette.selected = { 'ctermfg': '81', 'guifg': '#7aa880', 'cterm': 'bold,underline', 'gui': 'bold,underline' }
let s:palette.current_selection = { 'ctermbg': '236', 'guibg': '#4a4a4b', 'cterm': 'bold', 'gui': 'bold' }

let s:palette.selected_sign = { 'ctermfg': '196', 'guifg': '#d58888' }
let s:palette.current_selection_sign = s:palette.selected_sign
let g:clap#themes#tempus_classic#palette = s:palette

let g:clap_fuzzy_match_hl_groups = [
      \ [22, '#6e9cb0'],
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
      \ [63, '#7aa880'],
      \]
let &cpoptions = s:save_cpo
unlet s:save_cpo

