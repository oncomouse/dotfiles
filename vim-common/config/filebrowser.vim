" File Viewers:
" NerdTree {{
" Load NerdTree when opening a directory
  "autocmd StdinReadPre * let s:std_in=1
  "autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | endif
  " Toggle NerdTree
  " map <C-\> :NERDTreeToggle<CR>
"}}
" NetRW {{
  " let g:netrw_browse_split = 3 " Open in new tab
  " let g:netrw_altv = 1
  " let g:netrw_winsize = 33
  " map <C-\> :Sexplore<CR>
  " Open multiple files using visual mode in netrw
  "
  " https://vi.stackexchange.com/questions/13344/open-multiple-files-in-tabs-from-explore-mode
  function! NetrwOpenMultiTab(current_line,...) range
     " Get the number of lines.
     let n_lines =  a:lastline - a:firstline + 1

     " This is the command to be built up.
     let command = "normal "

     " Iterator.
     let i = 1

     " Virtually iterate over each line and build the command.
     while i < n_lines
        let command .= "tgT:" . ( a:firstline + i ) . "\<CR>:+tabmove\<CR>"
        let i += 1
     endwhile
     let command .= "tgT"

     " Restore the Explore tab position.
     if i != 1
        let command .= ":tabmove -" . ( n_lines - 1 ) . "\<CR>"
     endif

     " Restore the previous cursor line.
     let command .= ":" . a:current_line  . "\<CR>"

     " Check function arguments
     if a:0 > 0
        if a:1 > 0 && a:1 <= n_lines
           " The current tab is for the nth file.
           let command .= ( tabpagenr() + a:1 ) . "gt"
        else
           " The current tab is for the last selected file.
           let command .= (tabpagenr() + n_lines) . "gt"
        endif
     endif
     " The current tab is for the Explore tab by default.

     " Execute the custom command.
     execute command
  endfunction

  " Define mappings.
  augroup NetrwOpenMultiTabGroup
     autocmd!
     autocmd Filetype netrw vnoremap <buffer> <silent> <expr> t ":call NetrwOpenMultiTab(" . line(".") . "," . "v:count)\<CR>"
     autocmd Filetype netrw vnoremap <buffer> <silent> <expr> T ":call NetrwOpenMultiTab(" . line(".") . "," . (( v:count == 0) ? '' : v:count) . ")\<CR>"
   augroup END
"}}
