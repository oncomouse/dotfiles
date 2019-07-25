let g:neomake_go_gofmt_maker = {
    \ 'exe': 'gofmt',
    \ 'args': ['-l'],
    \ 'errorformat':
        \ '%f:%l:%c: %m,' .
        \ '%-G%.%#'
\ }
call neomake#configure#automake('nrw', 1000)
let g:neomake_open_list = 2
let g:neomake_go_enabled_makers = ['govet', 'golint', 'gofmt']
let g:neomake_javascript_enabled_makers = ['eslint']
let b:neomake_javascript_eslint_exe = GetNpmBin('eslint')
