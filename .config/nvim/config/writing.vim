" Writing:
let g:polyglot_disabled = ['markdown']
" Pandoc {{
  " Set up vim-pandoc's bibtex support
  let g:pandoc#biblio#bibs=['/Users/apilsch/Documents/Academic Stuff/library.bib']
  " Turn off folding
  let g:pandoc#modules#disabled = ["folding"]
  " Turn off conceal
  let g:pandoc#syntax#conceal#use = 0
  augroup markdowncommands
    " autocmd BufRead,BufNewFile *.md imap <buffer> <C-i> <Esc><localleader>iwi
    " autocmd BufRead,BufNewFile *.md imap <buffer> <C-b> <Esc><localleader>bwi
  augroup END

"}}
" Pandoc-After {{
  " Use deoplete for completion
  " let g:pandoc#after#modules#enabled = ["deoplete"]
  " call deoplete#custom#var('omni', 'input_patterns', {
    " \ 'pandoc': '@\w*',
    " \ })
  " call deoplete#custom#source('omni', 'functions', {
    " \ 'pandoc': 'pandoc#completion#Complete',
    " \ })
"}}
" Ditto {{
  au FileType markdown DittoOn
  nmap <leader>di <Plug>ToggleDitto
"}}
" Limelight {{
  let g:limelight_conceal_ctermfg="black"
"}}
" Goyo {{
  function! s:goyo_enter()
    " Trigger Limelight
    Limelight
    " For some reason, lightline-bufferline causes lightline to reenable, so
    " we have to turn it off on these events:
    augroup lightline_goyo
      autocmd BufWritePost,TextChanged,TextChangedI * call lightline#disable()
    augroup END
  endfunction

  function! s:goyo_leave()
    Limelight!
    augroup lightline_goyo
      autocmd!
    augroup END
  endfunction

  autocmd! User GoyoEnter call <SID>goyo_enter()
  autocmd! User GoyoLeave call <SID>goyo_leave()
"}}
" Easy Align {{
  " Start interactive EasyAlign in visual mode (e.g. vipga)
  xmap ga <Plug>(EasyAlign)
  " Start interactive EasyAlign for a motion/text object (e.g. gaip)
  nmap ga <Plug>(EasyAlign)
"}}
" Pencil {{
  augroup pencil
    autocmd!
    autocmd FileType text,markdown,mkd call lexical#init()
                                   \ | call litecorrect#init()
                                   \ | call textobj#sentence#init()
                                   \ | call pencil#init()
                                   " \ | call textobj#quote#init()
    " autocmd FileType text         call pencil#init()
  augroup END
  " VimForWriters recommended Pencil config: 
  let g:pencil#wrapModeDefault = 'soft'
  let g:pencil#textwidth = 74
  let g:pencil#joinspaces = 0 " I *think* this is what's fixing the weird problem we were having
  let g:pencil#cursorwrap = 1
  let g:pencil#conceallevel = 3
  let g:pencil#concealcursor = 'c'
  let g:pencil#softDetectSample = 20
  let g:pencil#softDetectThreshold = 130
  " source: http://www.naperwrimo.org/wiki/index.php?title=Vim_for_Writers
"}}
