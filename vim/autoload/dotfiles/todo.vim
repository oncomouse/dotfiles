" Next/Previous Projects
function! dotfiles#todo#NextProject() abort
  return search('^\t*\zs.\+:\(\s\+@[^\s(]\+\(([^)]*)\)\?\)*$', 'w')
endfunction

function! dotfiles#todo#PrevProject() abort
  return search('^\t*\zs.\+:\(\s\+@[^\s(]\+\(([^)]*)\)\?\)*$', 'bw')
endfunction
" Search
function! dotfiles#todo#SearchProject(project, depth, begin, end) abort
    call cursor(a:begin, 1)
    return search('\v^\t{' . a:depth . '}\V' . a:project . ':', 'c', a:end)
endfunction
function! dotfiles#todo#SearchEndOfItem(...) abort
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
function! dotfiles#todo#SearchProjects(projects) abort
    if empty(a:projects)
        return 0
    endif

    let save_pos = getpos('.')

    let begin = 1
    let end = line('$')
    let depth = 0

    for project in a:projects
        if !todo#SearchProject(project, depth, begin, end)
            call setpos('.', save_pos)
            return 0
        endif

        let begin = line('.')
        let end = dotfiles#todo#SearchEndOfItem(begin)
        let depth += 1
    endfor

    call cursor(begin, 1)
    normal! ^

    return begin
endfunction
function! dotfiles#todo#CompleteProject(lead, cmdline, pos) abort
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

function! dotfiles#todo#GoToProject() abort
  let res = input('Project: ', '', 'customlist,CompleteProject')

  if res !=# ''
    call dotfiles#todo#SearchProjects(split(res, ':'))
  endif
endfunction
