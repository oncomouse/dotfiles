call plug#begin()
" Themes:
Plug 'mhartington/oceanic-next'
" General Editing Plugins:
Plug 'sickill/vim-pasta' " Indentation-forward pasting
Plug 'editorconfig/editorconfig-vim' " Support editor configuration
Plug 'easymotion/vim-easymotion' " <leader><leader><motion key> for fast motion
Plug 'AndrewRadev/splitjoin.vim' " gS & gJ to split and join one-liners / functions
Plug 'ntpeters/vim-better-whitespace'
Plug 'tpope/vim-surround' " Change surrounding characters, see cheat sheet
Plug 'nathanaelkane/vim-indent-guides' " :IndentGuidesEnable to see indentation guides
Plug 'tpope/vim-repeat' " Repeat plugin commands
Plug 'djoshea/vim-autoread' " Better auto-reloading support
Plug 'MarcWeber/vim-addon-mw-utils' " I think this is required by something else
Plug 'tpope/vim-commentary' " Comment w/ gcc or gc (visual)
Plug 'amix/open_file_under_cursor.vim' " gf to open path at cursor
Plug 'terryma/vim-expand-region' " + or _ to expand or shrink (visual)
Plug 'airblade/vim-rooter' " Set project root
" Plug 'michaeljsmith/vim-indent-object'
" Syntax:
Plug 'sheerun/vim-polyglot'
Plug 'neoclide/vim-jsx-improve' " Better than what's included
" Plug 'styled-components/vim-styled-components'
" Plug 'leshill/vim-json'
" Plug 'isRuslan/vim-es6'
" Plug 'posva/vim-vue'
" Plug 'mxw/vim-jsx' ", { 'for': ['js', 'jsx' ] }
" Plug 'pangloss/vim-javascript' ", { 'for': ['js', 'jsx', 'vue' ] }
" Plug 'dag/vim-fish'
" Plug 'cakebaker/scss-syntax.vim'
" Plug 'vim-python/python-syntax'
" Fuzzy Search:
Plug 'Shougo/neomru.vim' " Most Recently Used file list (<leader>r to see
Plug 'brookhong/ag.vim' " :Ag --<filetype> <regex>
Plug '/usr/local/opt/fzf'
Plug 'junegunn/fzf.vim'
Plug 'Valloric/YouCompleteMe', { 'do': './install.py --ts-completer --go-completer' }
" Git Support:
Plug 'tpope/vim-fugitive' " :Gstatus for git statis; - to add, cc to commit
Plug 'airblade/vim-gitgutter'
" Writing:
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'vim-pandoc/vim-pandoc'
Plug 'junegunn/limelight.vim'
Plug 'junegunn/goyo.vim'
Plug 'reedes/vim-pencil'
Plug 'reedes/vim-lexical'
Plug 'reedes/vim-litecorrect'
Plug 'kana/vim-textobj-user'
Plug 'reedes/vim-textobj-sentence'
" Plug 'reedes/vim-textobj-quote'
Plug 'junegunn/vim-easy-align' " ga will align blocks to indicated character
" Web Development:
Plug 'chrisbra/Colorizer' ", { 'for': ['html', 'vue', 'jsx', 'js', 'css', 'scss'] }
Plug 'maksimr/vim-jsbeautify' ", { 'for': ['html', 'vue', 'jsx', 'js', 'css', 'scss'] }
Plug 'mattn/emmet-vim' ", { 'for': ['html', 'vue', 'jsx', 'js'] }
" File Viewers:
Plug 'ryanoasis/vim-devicons'
" Plug 'scrooloose/nerdtree'
" Plug 'Xuyuanp/nerdtree-git-plugin'
" Plug 'tpope/vim-vinegar'
" Statusline:
Plug 'itchyny/lightline.vim'
" Plug 'mgee/lightline-bufferline'
" Linter:
Plug 'w0rp/ale' ", { 'for': ['js', 'jsx', 'vue'] }
Plug 'maximbaz/lightline-ale' ", { 'for': ['js', 'jsx', 'vue'] }
" Clojure:
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
Plug 'kovisoft/paredit', { 'for': 'clojure' }
Plug 'weavejester/cljfmt', { 'for': 'clojure' }
Plug 'kien/rainbow_parentheses.vim'
Plug 'guns/vim-clojure-static', { 'for': 'clojure' }
call plug#end()
" Set <leader> and <localleader>:
let mapleader = "\<Space>"
let maplocalleader = "\\"

syntax on
filetype plugin indent on

set mouse=a
set clipboard=unnamed
set autoread
set autowrite
set hidden " turn off buffer saving when switching

if exists('+termguicolors')
  let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif

" Shortcut :tn for :tabnew
ca tn tabnew

filetype plugin indent on
set tabstop=8
set shiftwidth=2
set softtabstop=2
set expandtab

" Different highlighting for long lines:
let &colorcolumn=join(range(81,999),",")
highlight ColorColumn ctermbg=235 guibg=#182933
" Theme:
set background=dark
let g:oceanic_next_terminal_italic = 1
colorscheme OceanicNext
" Statusline:
" Lightline {{
  set laststatus=2
  set showtabline=2
  " let g:lightline#ale#indicator_checking = "\uf110"
  " let g:lightline#ale#indicator_warnings = "\uf071"
  " let g:lightline#ale#indicator_errors = "\uf05e"
  let g:lightline#ale#indicator_ok = "\uf00c"
  let g:lightline = {
        \ 'colorscheme': 'oceanicnext',
        \ 'active': {
        \   'left': [ [ 'mode', 'paste' ],
        \             [ 'gitbranch', 'gitgutter', 'readonly', 'filename', 'modified' ] ],
        \   'right': [ [ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok' ],
        \            [ 'lineinfo' ],
        \            [ 'wordcount' ], ]
        \ },
        \ 'component_function': {
        \   'gitbranch': 'fugitive#head',
        \   'gitgutter': 'MyGitGutter',
        \   'wordcount': 'WordCount',
        \   'filetype': 'MyFiletype',
        \ },
        \ 'component_expand': {
        \   'linter_checking': 'lightline#ale#checking',
        \   'linter_warnings': 'lightline#ale#warnings',
        \   'linter_errors': 'lightline#ale#errors',
        \   'linter_ok': 'lightline#ale#ok',
        \   'buffers': 'lightline#bufferline#buffers',
        \ },
        \ 'component_type': {
        \     'linter_checking': 'left',
        \     'linter_warnings': 'warning',
        \     'linter_errors': 'error',
        \     'linter_ok': 'left',
        \     'buffers': 'tabsel',
        \ },
        \ 'separator': { 'left': '', 'right': '' },
        \ 'subseparator': { 'left': '', 'right': '' }
        \ }
  let g:lightline.tab_component_function = {
        \   'filetype': 'MyTabFiletype',
        \   'mytabname': 'MyTabName',
        \   'modified': 'MyModified',
        \ }
  let g:lightline.tabline = {'left': [['tabs']], 'right': []}
  " let g:lightline.tabline = {'left': [['buffers']], 'right': []}
  let g:lightline.tab = {
        \ 'active': [ 'tabnum', 'filetype', 'mytabname', 'modified' ],
        \ 'inactive': [ 'tabnum', 'filetype', 'mytabname', 'modified' ] }
  set guioptions-=e
  " Display tab name, but use directory name if an index.js file is present:
  " Sourced from airline-vim
  function! MyTabName(n)
    let buflist = tabpagebuflist(a:n)
    let winnr = tabpagewinnr(a:n)
    let buf = expand('#'.buflist[winnr - 1])
    let filename = fnamemodify(buf, ":t")
    " expand('#'.buflist[winnr - 1].':t') " buflist[winnr - 1]
    if filename == 'index.js' || filename == 'index.jsx' || filename == 'index.ts' || filename == 'index.tsx'
      return fnamemodify(buf, ':p:h:t') . '/i'
    endif
    return filename
  endfunction
  " ✎
  function! MyModified(n)
    let winnr = tabpagewinnr(a:n)
    return gettabwinvar(a:n, winnr, '&modified') ? '✎' : gettabwinvar(a:n, winnr, '&modifiable') ? '' : ''
  endfunction
  " Display current filetype in tab name:
  function! MyTabFiletype(n)
    let buflist = tabpagebuflist(a:n)
    let winnr = tabpagewinnr(a:n)
    let buf = expand('#'.buflist[winnr - 1])
    return winwidth(0) > 70 ? (strlen(&filetype) ? WebDevIconsGetFileTypeSymbol(buf) : '') : ''
  endfunction

  " Display Current Filetype in Status Bar
  function! MyFiletype()
    return winwidth(0) > 70 ? (strlen(&filetype) ? WebDevIconsGetFileTypeSymbol() : '') : ''
  endfunction

  " WordCount
  let g:word_count="<unknown>"
  fun! WordCount()
      return g:word_count
  endfun
  fun! UpdateWordCount()
      let s = system("wc -w ".expand("%p"))
      let parts = split(s, ' ')
      if len(parts) > 1
          let g:word_count = parts[0]
      endif
  endfun
  augroup WordCounter
      au! CursorHold * call UpdateWordCount()
      au! CursorHoldI * call UpdateWordCount()
  augroup END

  function! MyGitGutter()
    if ! exists('*GitGutterGetHunkSummary')
          \ || ! get(g:, 'gitgutter_enabled', 0)
          \ || winwidth('.') <= 90
      return ''
    endif
    let symbols = [
          \ g:gitgutter_sign_added,
          \ g:gitgutter_sign_modified,
          \ g:gitgutter_sign_removed,
          \ ]
    let hunks = GitGutterGetHunkSummary()
    let ret = []
    for i in [0, 1, 2]
      if hunks[i] > 0
        call add(ret, symbols[i] . hunks[i])
      endif
    endfor
    return join(ret, ' ')
  endfunction
"}}
"Lightline Buffer {{
  " let g:lightline#bufferline#enable_devicons = 1
  " let g:lightline#bufferline#unicode_symbols = 1
  " let g:lightline#bufferline#filename_modifier = ':s?index\.js?i?:.'
  " let g:lightline#bufferline#show_number = 2

  " Buffer controls:
  " nmap <Leader>1 <Plug>lightline#bufferline#go(1)
  " nmap <Leader>2 <Plug>lightline#bufferline#go(2)
  " nmap <Leader>3 <Plug>lightline#bufferline#go(3)
  " nmap <Leader>4 <Plug>lightline#bufferline#go(4)
  " nmap <Leader>5 <Plug>lightline#bufferline#go(5)
  " nmap <Leader>6 <Plug>lightline#bufferline#go(6)
  " nmap <Leader>7 <Plug>lightline#bufferline#go(7)
  " nmap <Leader>8 <Plug>lightline#bufferline#go(8)
  " nmap <Leader>9 <Plug>lightline#bufferline#go(9)
  " nmap <Leader>0 <Plug>lightline#bufferline#go(10)
"}}

" General Editing Plugins:
" Syntax:
" Make sure .md files are read as Markdown:
" augroup markdown
"   autocmd!
"   autocmd BufNewFile,BufReadPost *.md set filetype=markdown
" augroup END
call ale#linter#Define('pandoc', {
\   'name': 'vale',
\   'executable': 'vale',
\   'command': 'vale --output=JSON %t',
\   'callback': 'ale#handlers#vale#Handle',
\})

" Use vim-jsx-improved instead:
let g:polyglot_disabled = ['jsx']

" Start with indent guides on:
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_auto_colors = 0
augroup indent-colors
  autocmd!

  autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  guibg=#1B2B34 ctermbg=235
  autocmd VimEnter,Colorscheme * :hi IndentGuidesEven guibg=#1C313D ctermbg=234
augroup END

" Setup rainbow-parentheses:
augroup rainbow-parentheses
  autocmd!
  autocmd VimEnter * RainbowParenthesesToggle
  autocmd Syntax * RainbowParenthesesLoadRound
  autocmd Syntax * RainbowParenthesesLoadSquare
  autocmd Syntax * RainbowParenthesesLoadBraces
augroup END

" Turn off autoroot for non-project files:
let g:rooter_patterns = ['project.clj', 'Rakefile', 'package.json', '.git/']
" let g:rooter_change_directory_for_non_project_files = '' " can be current or home
" let g:rooter_use_lcd = 1 " only change the current window
" CTRL-P {{
  " ctrlp support
  " let g:ctrlp_brief_prompt=1
  " let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git'
  " let g:ctrlp_show_hidden = 1
  " let g:ctrlp_prompt_mappings = {
  "     \ 'AcceptSelection("h")': ['<c-x>', '<c-s>'],
  "     \ 'AcceptSelection("e")': ['<c-t>',],
  "     \ 'AcceptSelection("t")': ['<cr>', '<2-LeftMouse>'],
  "     \ }
"}}
" FZF {{

  nnoremap <c-p> :FZF<cr>
  let g:fzf_nvim_statusline = 0 " disable statusline overwriting

  nnoremap <silent> <leader>f :Files<CR>
  nnoremap <silent> <leader>a :Buffers<CR>
  nnoremap <silent> <leader>A :Windows<CR>
  nnoremap <silent> <leader>; :BLines<CR>
  nnoremap <silent> <leader>o :BTags<CR>
  nnoremap <silent> <leader>O :Tags<CR>
  nnoremap <silent> <leader>? :History<CR>
  nnoremap <silent> <leader>/ :execute 'Ag ' . input('Ag/')<CR>
  nnoremap <silent> <leader>. :AgIn
  nnoremap <silent> <leader>r :call fzf#run({
    \   'source': 'sed "1d" $HOME/.cache/neomru/file',
    \   'sink': 'e '
    \ })<CR>

  nnoremap <silent> K :call SearchWordWithAg()<CR>
  vnoremap <silent> K :call SearchVisualSelectionWithAg()<CR>
  nnoremap <silent> <leader>gl :Commits<CR>
  nnoremap <silent> <leader>ga :BCommits<CR>
  nnoremap <silent> <leader>ft :Filetypes<CR>

  " FZF BibTeX Configuration
  let $FZF_BIBTEX_CACHEDIR = '/var/tmp'
  let $FZF_BIBTEX_SOURCES = '/Users/apilsch/Dropbox/Documents/Academic Stuff/library.bib'
  function! s:bibtex_cite_sink(lines)
    let r=system("bibtex-cite ", a:lines)
    execute ':normal! i' . r
  endfunction

  nnoremap <C-c> :call fzf#run({
              \ 'source': 'bibtex-ls',
              \ 'sink*': function('<sid>bibtex_cite_sink'),
              \ 'up': '40%',
              \ 'options': '--ansi --layout=reverse-list --multi --prompt "Cite> "'
              \ })<CR>

  function! s:bibtex_cite_sink_insert(lines)
    let r=system("bibtex-cite ", a:lines)
    execute ':normal! i' . r
    call feedkeys('a', 'n')
  endfunction

  inoremap <silent> <C-c> <c-g>u<c-o>:call fzf#run({
            \ 'source': 'bibtex-ls',
            \ 'sink*': function('<sid>bibtex_cite_sink_insert'),
            \ 'up': '40%',
            \ 'options': '--ansi --layout=reverse-list --multi --prompt "Cite> "'
            \ })<CR>

  imap <C-x><C-f> <plug>(fzf-complete-file-ag)
  imap <C-x><C-l> <plug>(fzf-complete-line)

  function! SearchWordWithAg()
    execute 'Ag' expand('<cword>')
  endfunction

  function! SearchVisualSelectionWithAg() range
    let old_reg = getreg('"')
    let old_regtype = getregtype('"')
    let old_clipboard = &clipboard
    set clipboard&
    normal! ""gvy
    let selection = getreg('"')
    call setreg('"', old_reg, old_regtype)
    let &clipboard = old_clipboard
    execute 'Ag' selection
  endfunction

  function! SearchWithAgInDirectory(...)
    call fzf#vim#ag(join(a:000[1:], ' '), extend({'dir': a:1}, g:fzf#vim#default_layout))
  endfunction
  command! -nargs=+ -complete=dir AgIn call SearchWithAgInDirectory(<f-args>)
"}}

" Git Support:
" Git Gutter {{
  let g:gitgutter_sign_added = "✚"
  let g:gitgutter_sign_modified = "…"
  let g:gitgutter_sign_removed = "✖"
  let g:gitgutter_sign_removed_first_line = "✖"
  let g:gitgutter_sign_modified_removed = "…"
"}}
" Writing:
let g:polyglot_disabled = ['markdown']
" Pandoc {{
  " Set up vim-pandoc's bibtex support
  let g:pandoc#biblio#bibs=['/Users/apilsch/Dropbox/Documents/Academic Stuff/library.bib']
  " Turn off folding
  let g:pandoc#modules#disabled = ["folding"]
  " Turn off conceal
  let g:pandoc#syntax#conceal#use = 0
  " augroup markdowncommands
    " autocmd BufRead,BufNewFile *.md imap <buffer> <C-i> <Esc><localleader>iwi
    " autocmd BufRead,BufNewFile *.md imap <buffer> <C-b> <Esc><localleader>bwi
  " augroup END

  if !exists('g:ycm_semantic_triggers')
    let g:ycm_semantic_triggers = {}
  endif
  let g:ycm_semantic_triggers.markdown = ['@']
  let g:ycm_semantic_triggers.pandoc = ['@']
  let g:ycm_filetype_blacklist = {}
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
" Web Development:
" Colorizer {{
let g:colorizer_auto_filetype='css,scss,sass,less,html'
" }}
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
" Highlight a block and type "@" to run a macro on the block:
xnoremap @ :<C-u>call ExecuteMacroOverVisualRange()<CR>

function! ExecuteMacroOverVisualRange()
  echo "@".getcmdline()
  execute ":'<,'>normal @".nr2char(getchar())
endfunction
" Linter:
" ALE {{
  " Better ALE Msg Format
  let g:ale_echo_msg_error_str = 'E'
  let g:ale_echo_msg_warning_str = 'W'
  let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
  " Jump between ALE Errors:
  nmap <silent> <C-k> <Plug>(ale_previous_wrap)
  nmap <silent> <C-j> <Plug>(ale_next_wrap)
  " Lint only on save:
  " let g:ale_lint_on_text_changed = 'never'
  " let g:ale_lint_on_enter = 1
"}}

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

