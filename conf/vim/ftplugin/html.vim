let b:ale_linters = ['htmlhint']
let b:ale_fixers = ['html-beautify']
let b:ale_html_beautify_options = '-s 2'
set matchpairs+=<:>
set formatprg=html-beautify\-s\ 2
