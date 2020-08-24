" Prettier for Lua
" Need npm i -g prettier @prettier/plugin-lua

function! ale#fixers#prettier_lua#Fix(buffer) abort
    return ale#semver#RunWithVersionCheck(
    \   a:buffer,
    \   ale#fixers#prettier#GetExecutable(a:buffer),
    \   '%e --version',
    \   function('ale#fixers#prettier_lua#ApplyFixForVersion'),
    \)
endfunction

function! ale#fixers#prettier_lua#ApplyFixForVersion(buffer, version) abort
    let l:executable = ale#fixers#prettier#GetExecutable(a:buffer)
    let l:options = ale#Var(a:buffer, 'javascript_prettier_options')
    let l:parser = ''

    let l:filetypes = split(getbufvar(a:buffer, '&filetype'), '\.')

    if index(l:filetypes, 'handlebars') > -1
        let l:parser = 'glimmer'
    endif

    " Append the --parser flag depending on the current filetype (unless it's
    " already set in g:javascript_prettier_options).
    if empty(expand('#' . a:buffer . ':e')) && l:parser is# ''  && match(l:options, '--parser') == -1
        " Mimic Prettier's defaults. In cases without a file extension or
        " filetype (scratch buffer), Prettier needs `parser` set to know how
        " to process the buffer.
        if ale#semver#GTE(a:version, [1, 16, 0])
            let l:parser = 'babel'
        else
            let l:parser = 'babylon'
        endif

        let l:prettier_parsers = {
        \ 'lua':'lua',
        \}

        for l:filetype in l:filetypes
            if has_key(l:prettier_parsers, l:filetype)
                let l:parser = l:prettier_parsers[l:filetype]
                break
            endif
        endfor
    endif

    if !empty(l:parser)
        let l:options = (!empty(l:options) ? l:options . ' ' : '') . '--parser ' . l:parser
    endif

    " Special error handling needed for prettier_d
    if l:executable =~# 'prettier_d$'
        return {
        \   'command': ale#path#BufferCdString(a:buffer)
        \       . ale#Escape(l:executable)
        \       . (!empty(l:options) ? ' ' . l:options : '')
        \       . ' --stdin-filepath %s --stdin',
        \   'process_with': 'ale#fixers#prettier#ProcessPrettierDOutput',
        \}
    endif

    " 1.4.0 is the first version with --stdin-filepath
    if ale#semver#GTE(a:version, [1, 4, 0])
        return {
        \   'command': ale#path#BufferCdString(a:buffer)
        \       . ale#Escape(l:executable)
        \       . (!empty(l:options) ? ' ' . l:options : '')
        \       . ' --stdin-filepath %s --stdin',
        \}
    endif

    return {
    \   'command': ale#Escape(l:executable)
    \       . ' %t'
    \       . (!empty(l:options) ? ' ' . l:options : '')
    \       . ' --write',
    \   'read_temporary_file': 1,
    \}
endfunction
