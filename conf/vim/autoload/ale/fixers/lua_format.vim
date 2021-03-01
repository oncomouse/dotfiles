call ale#Set('lua_lua_format_executable', 'lua-format')
call ale#Set('lua_lua_format_options', '')

function! ale#fixers#lua_format#Fix(buffer) abort
    let l:executable = ale#Var(a:buffer, 'lua_lua_format_executable')
    let l:options = ale#Var(a:buffer, 'lua_lua_format_options')

    return {
    \   'command': ale#Escape(l:executable)
    \       . (empty(l:options) ? '' : ' ' . l:options),
    \}
endfunction

