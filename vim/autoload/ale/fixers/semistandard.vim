function! ale#fixers#semistandard#Fix(buffer) abort
    let l:executable = ale#fixers#standard#GetExecutable(a:buffer)
    let l:filetype = getbufvar(a:buffer, '&filetype')
    let l:options_type = 'javascript_standard_options'

    if l:filetype =~# 'typescript'
        let l:options_type = 'typescript_standard_options'
    endif

    let l:options = ale#Var(a:buffer, l:options_type)

    return {
    \   'command': ale#node#Executable(a:buffer, l:executable)
    \   . (!empty(l:options) ? ' ' . l:options : '')
    \       . ' --fix --stdin < %s > %t',
    \   'read_temporary_file': 1,
    \}
endfunction
