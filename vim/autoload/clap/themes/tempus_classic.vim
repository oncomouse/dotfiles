let s:save_cpo = &cpoptions
set cpoptions&vim

let s:palette = {}

let s:palette.display = { 'ctermbg': '235', 'guibg': '#312e30' }

" Let ClapInput, ClapSpinner and ClapSearchText use the same backgound.
let s:bg0 = { 'ctermbg': '60', 'guibg': '#4a4a4b' }
let s:palette.input = s:bg0
let s:palette.spinner = extend({ 'ctermfg': '11', 'guifg':'#b1942b', 'cterm': 'bold', 'gui': 'bold'}, s:bg0)
let s:palette.search_text = extend({ 'ctermfg': '195', 'guifg': '#aeadaf', 'cterm': 'bold', 'gui': 'bold' }, s:bg0)

let s:palette.preview = { 'ctermbg': '238', 'guibg': '#4a4a4b' }

let s:palette.selected = { 'ctermfg': '81', 'guifg': '#7aa880', 'cterm': 'bold,underline', 'gui': 'bold,underline' }
let s:palette.current_selection = { 'ctermbg': '236', 'guibg': '#4a4a4b', 'cterm': 'bold', 'gui': 'bold' }

let s:palette.selected_sign = { 'ctermfg': '196', 'guifg': '#d58888' }
let s:palette.current_selection_sign = s:palette.selected_sign
hi ClapFuzzyMatches1 guifg=#6e9cb0
hi ClapFuzzyMatches2 guifg=#6f9dab
hi ClapFuzzyMatches3 guifg=#709ea7
hi ClapFuzzyMatches4 guifg=#719fa2
hi ClapFuzzyMatches5 guifg=#72a09e
hi ClapFuzzyMatches6 guifg=#73a19a
hi ClapFuzzyMatches7 guifg=#74a295
hi ClapFuzzyMatches8 guifg=#75a391
hi ClapFuzzyMatches9 guifg=#76a48d
hi ClapFuzzyMatches10 guifg=#77a588
hi ClapFuzzyMatches11 guifg=#78a684
hi ClapFuzzyMatches12 guifg=#7aa880
let g:clap#themes#tempus_classic#palette = s:palette

let &cpoptions = s:save_cpo
unlet s:save_cpo
