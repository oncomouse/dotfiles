" Writing:
" Deoplete BibLaTeX source {{
	call deoplete#custom#option('sources', {
		\ 'pandoc': ['biblatex', 'buffer', 'file']
	\})
	call deoplete#custom#var('biblatex', 'addinfo', 0)
	call deoplete#custom#var('biblatex', 'bibfile', '/Users/apilsch/Dropbox/Documents/Academic Stuff/library.bib')
	call deoplete#custom#var('biblatex', 'filetypes', ['pandoc', 'markdown'])
" }}
" Pandoc {{
  " Uncomment to use the omni-func for bibliography completion:
  " let g:pandoc#biblio#bibs=['/Users/apilsch/Dropbox/Documents/Academic Stuff/library.bib']
  " Turn off folding and vim-pandoc's BibTeX support
  let g:pandoc#modules#disabled = ["folding", "bibliography"]
  " Turn off conceal
  let g:pandoc#syntax#conceal#use = 0
  " Turn on language support
  let g:pandoc#syntax#codeblocks#embeds#langs = [
        \ "javascript",
        \ "css",
        \ "json",
        \ "html",
        \ "scss",
        \]
  "
  " Deoplete omni-function bib completion:
  " call deoplete#custom#var('omni', 'input_patterns', {
  "   \ 'pandoc': '@\w*'
  " \})
  "
  " YouCompleteMe omni-function completion:
  " if !exists('g:ycm_semantic_triggers')
  "   let g:ycm_semantic_triggers = {}
  " endif
  " let g:ycm_semantic_triggers.markdown = ['@']
  " let g:ycm_semantic_triggers.pandoc = ['@']
  " let g:ycm_filetype_blacklist = {}
"}}
" Limelight {{
  let g:limelight_conceal_ctermfg="black"
"}}
" Goyo {{
  function! s:goyo_enter()
    " Trigger Limelight
    " Limelight
    " For some reason, lightline-bufferline causes lightline to reenable, so
    " we have to turn it off on these events:
    augroup lightline_goyo
      autocmd BufWritePost,TextChanged,TextChangedI * call lightline#disable()
    augroup END
  endfunction

  function! s:goyo_leave()
    " Limelight!
    augroup lightline_goyo
      autocmd!
    augroup END
  endfunction

"}}
" Easy Align {{
  " Start interactive EasyAlign in visual mode (e.g. vipga)
  xmap ga <Plug>(EasyAlign)
  " Start interactive EasyAlign for a motion/text object (e.g. gaip)
  nmap ga <Plug>(EasyAlign)
"}}
" Pencil {{
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
" Initialize our writing environment:
augroup writing
  autocmd!
  autocmd FileType pandoc,text,markdown call lexical#init()
                                 \ | call litecorrect#init()
                                 \ | call textobj#sentence#init()
                                 \ | call pencil#init()
                                 \ | Limelight
  autocmd FileType text,pandoc,markdown  call pencil#init()
  " Make sure j and k work with word wrap turned on:
  autocmd FileType text,pandoc,markdown nmap j gj
  autocmd FileType text,pandoc,markdown nmap k gk
  autocmd! User GoyoEnter call <SID>goyo_enter()
  autocmd! User GoyoLeave call <SID>goyo_leave()
augroup END
