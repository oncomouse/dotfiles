" Highlighted Yank:
autocmd dotfiles-settings TextYankPost * silent! lua vim.highlight.on_yank {higroup="IncSearch", timeout=500}

" Close Preview Window:
autocmd dotfiles-settings CompleteDone * if pumvisible() == 0 | pclose | endif

" On opening a file, jump to the last known cursor position (see :h line())
autocmd dotfiles-settings BufReadPost *
\ if line("'\"") > 1 && line("'\"") <= line("$") && &ft !~# 'commit' |
\	 exe "normal! g`\"" |
\ endif 

" Fix window resizing
autocmd dotfiles-settings VimEnter * silent exec "!kill -s SIGWINCH $PPID"

" Update FASD For NeoVim: {{{
function! s:fasd_update() abort
	if empty(&buftype)
		call jobstart(['fasd', '-A', expand('%:p')])
	endif
endfunction
autocmd dotfiles-settings BufWinEnter,BufFilePost * call s:fasd_update()
" }}}
" # vim:foldmethod=marker
