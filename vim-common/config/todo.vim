" This is many of the commands from taskpaper.vim but set to load on my 
" todo.txt file and using my done notation. Also, turns off all the
" formatting.
augroup todo
  autocmd!
  " Mark a task as done:
  autocmd BufRead,BufNewFile todo.txt nnoremap <buffer> <leader>td A X<esc>
  autocmd BufRead,BufNewFile todo.txt vnoremap <buffer> <leader>td A X<esc>
  " Go To Project
  autocmd BufRead,BufNewFile todo.txt nnoremap <buffer> <leader>tg :call GoToProject()<CR>
  " Search for done tasks:
  autocmd BufRead,BufNewFile todo.txt nnoremap <buffer> <leader>t/ / X$<CR>
  " Go To Next Project:
  autocmd BufRead,BufNewFile todo.txt nnoremap <buffer> <leader>tj :call NextProject()<CR>
  " Go To Previous Project:
  autocmd BufRead,BufNewFile todo.txt nnoremap <buffer> <leader>tk :call PrevProject()<CR>
augroup END
" Next/Previous Projects
function! NextProject()
  return search('^\t*\zs.\+:\(\s\+@[^\s(]\+\(([^)]*)\)\?\)*$', 'w')
endfunction

function! PrevProject()
  return search('^\t*\zs.\+:\(\s\+@[^\s(]\+\(([^)]*)\)\?\)*$', 'bw')
endfunction
" Search
function! SearchProject(project, depth, begin, end)
    call cursor(a:begin, 1)
    return search('\v^\t{' . a:depth . '}\V' . a:project . ':', 'c', a:end)
endfunction
function! SearchEndOfItem(...)
    let lnum = a:0 > 0 ? a:1 : line('.')
    let flags = a:0 > 1 ? a:2 : ''

    let depth = len(matchstr(getline(lnum), '^\t*'))

    let end = lnum
    let lnum += 1
    while lnum <= line('$')
        let line = getline(lnum)

        if line =~ '^\s*$'
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
function! SearchProjects(projects)
    if empty(a:projects)
        return 0
    endif

    let save_pos = getpos('.')

    let begin = 1
    let end = line('$')
    let depth = 0

    for project in a:projects
        if !SearchProject(project, depth, begin, end)
            call setpos('.', save_pos)
            return 0
        endif

        let begin = line('.')
        let end = SearchEndOfItem(begin)
        let depth += 1
    endfor

    call cursor(begin, 1)
    normal! ^

    return begin
endfunction
function! CompleteProject(lead, cmdline, pos)
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

function! GoToProject()
  let res = input('Project: ', '', 'customlist,CompleteProject')

  if res != ''
    call SearchProjects(split(res, ':'))
  endif
endfunction

