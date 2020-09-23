function! todo#toggle_done() abort
  let line=getline('.')
  if line =~# ' X$'
    call setline('.', substitute(line, ' X$', '', ''))
  elseif line =~# '\[ \]'
    call setline('.', substitute(line, '[ ]', '[X]', ''))
  elseif line =~# '\[X\]'
    call setline('.', substitute(line, '[X]', '[ ]', ''))
  else
    call setline('.', line . ' X')
  endif
endfunction
function! todo#next_project() abort
  return search('^\t*\zs.\+:\(\s\+@[^\s(]\+\(([^)]*)\)\?\)*$', 'w')
endfunction
function! todo#prev_project() abort
  return search('^\t*\zs.\+:\(\s\+@[^\s(]\+\(([^)]*)\)\?\)*$', 'bw')
endfunction
" Search
function! s:search_project(project, depth, begin, end) abort
    call cursor(a:begin, 1)
    return search('\v^\t{' . a:depth . '}\V' . a:project . ':', 'c', a:end)
endfunction
function! s:search_end_of_item(...) abort
    let lnum = a:0 > 0 ? a:1 : line('.')
    let flags = a:0 > 1 ? a:2 : ''

    let depth = len(matchstr(getline(lnum), '^\t*'))

    let end = lnum
    let lnum += 1
    while lnum <= line('$')
        let line = getline(lnum)

        if line =~# '^\s*$'
            " Do nothing
        elseif depth < len(matchstr(line, '^\t*'))
            let end = lnum
        else
            break
        endif

        let lnum += 1
    endwhile

    if flags !~# 'n'
        call cursor(end, 0)
        normal! ^
    endif

    return end
endfunction
function! s:search_projects(projects) abort
    if empty(a:projects)
        return 0
    endif

    let save_pos = getpos('.')

    let begin = 1
    let end = line('$')
    let depth = 0

    for project in a:projects
        if !s:search_project(project, depth, begin, end)
            call setpos('.', save_pos)
            return 0
        endif

        let begin = line('.')
        let end = s:search_end_of_item(begin)
        let depth += 1
    endfor

    call cursor(begin, 1)
    normal! ^

    return begin
endfunction
function! todo#CompleteProject(lead, cmdline, pos) abort
    let lnum = 1
    let list = []
    let stack = ['']
    let depth = 0

    while lnum <= line('$')
        let line = getline(lnum)
        let ml = matchlist(line, '\v\C^\t*(.+):(\s+\@[^ \t(]+(\([^)]*\))?)*$')

        if !empty(ml)
            let d = len(matchstr(line, '^\t*'))

            while d < depth
                call remove(stack, -1)
                let depth -= 1
            endwhile

            while d > depth
                call add(stack, '')
                let depth += 1
            endwhile

            let stack[d] = ml[1]

            let candidate = join(stack, ':')
            if candidate =~ '^' . a:lead
                call add(list, join(stack, ':'))
            endif
        endif

        let lnum += 1
    endwhile

    return list
endfunction

function! todo#goto_project() abort
  let res = input('Project: ', '', 'customlist,CompleteProject')

  if res !=# ''
    call s:search_projects(split(res, ':'))
  endif
endfunction
