if !(&runtimepath =~# expand('~/dotfiles/conf/vim/').',')
  let &runtimepath .= ','.expand('~/dotfiles/conf/vim/')
endif
" Mac NeoVim Settings: {{{
  if has('mac') && has('nvim')
    let g:python_host_prog='/usr/bin/python2.7'
    let g:python3_host_prog='/usr/local/bin/python3'
    let g:ruby_host_prog=expand('~/.asdf/shims/neovim-ruby-host')
    let g:node_host_prog='/usr/local/lib/node_modules/neovim/bin/cli.js'
    " This is macOS only, I believe, but it fixes slow start-up for clipboard:
    let g:clipboard = {
          \'copy': {'+': 'pbcopy', '*': 'pbcopy'},
          \'paste': {'+': 'pbpaste', '*': 'pbpaste'},
          \'name': 'pbcopy', 'cache_enabled': 0
          \}
  endif
" }}}
" Dotfiles Settings: {{{
  let g:dotfiles_mode = get(g:, 'dotfiles_mode', 'desktop')
  let g:complete_package = 'fzf' " fzf, telescope, or coc.nvim
  if g:dotfiles_mode ==# 'server' || (g:complete_package ==# 'telescope' && !has('nvim-0.5'))
    let g:complete_package = 'fzf'
  endif
  " This avoids highlighting big files:
  let g:large_file = 20*1024*1024
" }}}
" Basic Vim Settings: {{{
  set mouse=a " Mouse support
  if has('clipboard')
    if has('unnamedplus')
      set clipboard=unnamedplus,unnamed
    else
      set clipboard=unnamed
    endif
  endif

  " Load Basic Settings:
  runtime vimrc-minimal

  " Set Leader:
  let mapleader = "\<Space>"
  let maplocalleader = ','

  " Add Dotfiles After To RTP:
  let &runtimepath .= ','.expand('~/dotfiles/vim/after/')

  " Show Updates Of Commands:
  if has('nvim')
    set inccommand=split
  endif

  " Set Spellfile Location:
  set spellfile=~/dotfiles/conf/vim/spell/en.utf-8.add

  " Grep:
  if executable('rg')
    set grepprg=rg\ --vimgrep
  elseif executable('ag')
    set grepprg=ag\ --vimgrep
  endif

  " Completion:
  set completeopt=menuone,noselect,noinsert,preview
  " Shut off completion messages
  set shortmess+=c
  " prevent a condition where vim lags due to searching include files.
  set complete-=i

  " Statusline:
  set statusline=%!dotfiles#statusline#statusline()
" }}}
" Tabs: {{{
  set tabstop=8
  set shiftwidth=2
  set softtabstop=2
  set expandtab
  " Override Defaults: {{{
    augroup my-tabstops
      autocmd!
      " Go, Lua
      autocmd FileType markdown,c,cpp,go,lua setlocal tabstop=4
      autocmd FileType markdown,c,cpp,go,lua setlocal shiftwidth=4
      autocmd FileType markdown,c,cpp,go,lua setlocal noexpandtab
      " Python
      autocmd FileType python setlocal tabstop=8 expandtab shiftwidth=4 softtabstop=4
      " JavaScript
      autocmd FileType javascript,javascriptreact setlocal tabstop=2
      autocmd FileType javascript,javascriptreact setlocal shiftwidth=2
      autocmd FileType javascript,javascriptreact setlocal softtabstop=2
      autocmd FileType javascript,javascriptreact setlocal expandtab
    augroup END
  " }}}
" }}}
" Folds: {{{
  if g:dotfiles_mode ==# 'server'
    set nofoldenable " no folding in server mode
  else
    set foldmethod=syntax
    set foldlevel=99
    augroup custom-folds
      autocmd!
      autocmd FileType vim setlocal foldmethod=marker foldlevel=0
      autocmd FileType css setlocal foldmethod=syntax foldlevel=0
      autocmd FileType scss setlocal foldmethod=syntax foldlevel=0
      autocmd FileType diff setlocal nofoldenable
    augroup END
  endif
" }}}

function! s:minpac_path() abort
  let l:minpac = '/pack/minpac/opt/minpac'
  return (has('nvim') ? stdpath('data') . '/site' : expand('~/.vim')) . l:minpac 
endfunction

let g:user_install_path = s:minpac_path()
if empty(glob(g:user_install_path))
  silent execute '!git clone --depth 1 https://github.com/k-takata/minpac "'.g:user_install_path .'"'
end

function! PackInit() abort
  packadd minpac

  call minpac#init()
  call minpac#add('k-takata/minpac', {'type': 'opt'})
  call minpac#add('tpope/vim-sensible')
  " Get Started: {{{
    call minpac#add('xero/securemodelines') " Secure modelines
    set nomodeline
    let g:secure_modelines_verbose = 0
    let g:secure_modelines_modelines = 15
    let g:secure_modelines_allowed_items = [
        \ 'textwidth',   'tw',
        \ 'softtabstop', 'sts',
        \ 'tabstop',     'ts',
        \ 'shiftwidth',  'sw',
        \ 'expandtab',   'et',   'noexpandtab', 'noet',
        \ 'filetype',    'ft',
        \ 'foldmethod',  'fdm',
        \ 'foldlevel',   'fdl',
        \ 'readonly',    'ro',   'noreadonly', 'noro',
        \ 'rightleft',   'rl',   'norightleft', 'norl',
        \ 'cindent',     'cin',  'nocindent', 'nocin',
        \ 'smartindent', 'si',   'nosmartindent', 'nosi',
        \ 'autoindent',  'ai',   'noautoindent', 'noai',
        \ 'spell', 'nospell',
        \ 'spelllang',
        \ 'wrap', 'nowrap',
        \ 'syntax'
    \ ]
  " }}}
  " General Editing: {{{
    if !has('nvim')
      call minpac#add('noahfrederick/vim-neovim-defaults') " Neovim defaults for vim
    endif
    call minpac#add('sickill/vim-pasta') " Indentation-forward pasting
    call minpac#add('tpope/vim-repeat')
    call minpac#add('oncomouse/vim-surround') " ys to add, cs to change, ds to delete. f, F for function, t, T for tag
    call minpac#add('tpope/vim-commentary') " Comment w/ gcc or gc (visual)
    call minpac#add('airblade/vim-rooter') " Set project root
    let g:rooter_patterns = ['Rakefile', 'package.json', '.git/', 'Gemfile'
          \ , 'pyproject.toml', 'setup.py']
    call minpac#add('wellle/targets.vim') " add next block n]) targets, plus words in commas (a,), asterisks (a*), etc
    call minpac#add('cohama/lexima.vim') " Autopairs + Endwise
    function! s:make_rule(at, end, filetype, syntax)
      return {
      \ 'char': '<CR>',
      \ 'input': '<CR>',
      \ 'input_after': '<CR>' . a:end,
      \ 'at': a:at,
      \ 'except': '\C\v^(\s*)\S.*%#\n%(%(\s*|\1\s.+)\n)*\1' . a:end,
      \ 'filetype': a:filetype,
      \ 'syntax': a:syntax,
      \ }
    endfunction
    function! s:extend_endwise() abort
      " Lua endwise rules:
      call lexima#add_rule(s:make_rule('^\s*if\>.*then\%(.*[^.:@$]\<end\>\)\@!.*\%#', 'end', 'lua', []))
      call lexima#add_rule(s:make_rule('^\s*\%(for\|while\)\>.*do\%(.*[^.:@$]\<end\>\)\@!.*\%#', 'end', 'lua', []))
      call lexima#add_rule(s:make_rule('^\s*\%(local\)\=.*function\>\%(.*[^.:@$]\<end\>\)\@!.*\%#', 'end', 'lua', []))
    endfunction
    augroup lexima-startup
      autocmd!
      autocmd VimEnter * call s:extend_endwise()
    augroup END
    " inoremap <C-l> <C-r>=lexima#insmode#leave_till_eol("")<CR>
    if has('nvim-0.5')
      augroup highlight_yank
        autocmd!
        autocmd TextYankPost * silent! lua vim.highlight.on_yank {higroup="IncSearch", timeout=500}
      augroup END
    else
      call minpac#add('machakann/vim-highlightedyank') " Highlights yank
    endif
  " }}}
  " Git Support: {{{
    call minpac#add('lambdalisue/gina.vim') " :Gina status to schedule; :Gina commit to commit
    function! s:load_gina() abort
      call gina#custom#command#option('status', '--opener', &previewheight . 'split')
      call gina#custom#command#option('commit', '--opener', &previewheight . 'split')
      call gina#custom#command#option('diff', '--opener', &previewheight . 'split')
      call gina#custom#command#option('status', '--group', 'short')
      call gina#custom#command#option('commit', '--group', 'short')
      call gina#custom#command#option('diff', '--group', 'short')
      " Implement vim-fugitive commands in Gina:
      call gina#custom#mapping#nmap('status', 'cc', ':<C-u>Gina commit<CR>', {'noremap': 1, 'silent': 1})
    endfunction
    augroup gina-startup
      autocmd!
      autocmd VimEnter * call s:load_gina()
    augroup END
    cnoreabbrev gina Gina
  " }}}
  " List Support: {{{
    if g:complete_package ==# 'fzf'
      " macOS Homebrew
      if isdirectory('/usr/local/opt/fzf')
        call minpac#add('/usr/local/opt/fzf')
      " Arch
      elseif isdirectory('/usr/share/vim/vimfiles')
        let &runtimepath .= ',/usr/share/vim/vimfiles'
      " Local install
      elseif isdirectory('~/.fzf')
        call minpac#add('~/.fzf')
      " Fallback
      else
        " call minpac#add('junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all') }
      endif
      call minpac#add('junegunn/fzf.vim') " Add shorcuts for FZF
    elseif g:complete_package ==# 'telescope'
      call minpac#add('nvim-lua/popup.nvim')
      call minpac#add('nvim-lua/plenary.nvim')
      call minpac#add('nvim-telescope/telescope.nvim')
    endif
  " }}}
  " Server-Only {{{
    if !has('nvim') || g:dotfiles_mode ==# 'server'
    endif
  " }}}
  if g:dotfiles_mode ==# 'desktop'
    " General Editing: {{{
      if has('nvim')
        call minpac#add('norcalli/nvim-colorizer.lua') " HTML codes and HTML color words to colors
      endif
      if has('nvim-0.5')
        call minpac#add('windwp/nvim-ts-autotag', { 'branch': 'main' }) " Auotmatically close HTML tags
      else
        call minpac#add('alvan/vim-closetag') " Automatically close HTML tags
        let g:closetag_filenames = '*.html,*.xhtml,*.phtml,*.php,*.js,*.erb'
        let g:closetag_xhtml_filenames = '*.xhtml,*.js,*.erb'
        let g:closetag_filetypes = 'html,javascriptreact'
        let g:closetag_close_shortcut = '<leader>>'
        let g:closetag_regions = {
          \ 'typescriptreact': 'jsxRegion,tsxRegion',
          \ 'javascriptreact': 'jsxRegion',
          \ 'javascript': 'jsxRegion',
          \ }
      endif
      call minpac#add('Konfekt/FastFold') " Better fold support
      let g:fastfold_savehook = 1
      let g:fastfold_fold_command_suffixes =  ['x','X','a','A','o','O','c','C', 'r', 'R', 'm', 'M']
      let g:fastfold_fold_movement_commands = [']z', '[z', 'zj', 'zk']
      let g:fastfold_minlines = 0
      call minpac#add('Yggdroot/indentLine') " Indent with characters
      let g:indentLine_char = "\u22EE"
      let g:indentLine_fileTypeExclude = ['markdown', 'fzf', 'help']
      let g:indentLine_setColors = 0
      " endif
      call minpac#add('sk1418/QFGrep') " <leader>g to filter qf list, <leader>v to inverse filter, <leader>r to restore
    " }}}
      " Syntax: {{{
        if !has('nvim-0.5')
          call minpac#add('cespare/vim-toml') " TOML syntax
          call minpac#add('neoclide/vim-jsx-improve') " JSX (javascriptreact) Syntax
          call minpac#add('elzr/vim-json') " JSON Syntax
          let g:vim_json_syntax_conceal = 0
          call minpac#add('tbastos/vim-lua') " Lua Syntax
        endif
        call minpac#add('plasticboy/vim-markdown') " Markdown Syntax
        let g:vim_markdown_frontmatter = 1 " Format YAML
        let g:vim_markdown_strikethrough = 0 " Don't format strikethrough
        let g:vim_markdown_conceal = 0 " Don't conceal
        let g:vim_markdown_conceal_code_blocks = 0 " Don't conceal code blocks
        let g:vim_markdown_math = 1 " Do process MathJaX and LaTeX math
        call minpac#add('cakebaker/scss-syntax.vim') " SCSS Syntax
        call minpac#add('oncomouse/vim-fish') " Fish Syntax & Async Completion
        " call minpac#add('numirias/semshi', { 'do': ':UpdateRemotePlugins') } " Variable Highlighting for Python
      " }}}
      " Text Object Plugins: {{{
        call minpac#add('kana/vim-textobj-user') " Allow custom textobj definitions
        if !has('nvim-0.5')
          call minpac#add('kana/vim-textobj-function') " af, if, aF, iF select function
          call minpac#add('rhysd/vim-textobj-ruby') " ar for ruby objects
          call minpac#add('thinca/vim-textobj-function-javascript') " js function support
          call minpac#add('bps/vim-textobj-python') " function and class support for Python
        endif
      " }}}
      " Autocomplete: {{{
        if g:complete_package ==# 'fzf' && has('nvim-0.5')
          call minpac#add('gfanto/fzf-lsp.nvim', { 'branch': 'main' })
        endif
        " Load LSP + Completion:
        if g:complete_package =~# 'coc.nvim'
          call minpac#add('neoclide/coc.nvim', {'do': 'yarn install --frozen-lockfile'})
          let g:coc_config_home = expand('~/dotfiles/conf/vim/lsp-settings/coc.nvim/')
          let g:coc_global_extensions = [
          \   'coc-calc',
          \   'coc-css',
          \   'coc-diagnostic',
          \   'coc-eslint',
          \   'coc-fish',
          \   'coc-html',
          \   'coc-json',
          \   'coc-lists',
          \   'coc-prettier',
          \   'coc-python',
          \   'coc-solargraph',
          \   'coc-stylelintplus',
          \   'coc-styled-components',
          \   'coc-tsserver',
          \   'coc-vimlsp',
          \   'coc-yaml',
          \   'coc-yank',
          \]
        else
          " LSP Support: {{{
            if has('nvim-0.5')
              call minpac#add('neovim/nvim-lspconfig')
              call minpac#add('nvim-treesitter/nvim-treesitter')
              call minpac#add('nvim-treesitter/nvim-treesitter-textobjects')
              " call minpac#add('nvim-treesitter/playground')
            else
              call minpac#add('autozimu/LanguageClient-neovim', {
                \ 'branch': 'next',
                \ 'do': 'bash install.sh',
                \ })
              let g:LanguageClient_settingsPath = expand('~/dotfiles/conf/vim/lsp-settings/LanguageClient-neovim/settings.json')
              let g:LanguageClient_useVirtualText = 'CodeLens'
              " Turn off all diagnostic stuff (pump it all to ALE):
              let g:LanguageClient_diagnosticsEnable = 0
              " Debug:
              " let g:LanguageClient_loggingFile = expand('~/lc.log')
              " let g:LanguageClient_loggingLevel = 'DEBUG'
              " Always use hover:
              let g:LanguageClient_useFloatingHover = 0
              let g:LanguageClient_hoverPreview = 'Auto'
              " Root patterns:
              let g:LanguageClient_rootMarkers = g:rooter_patterns
              let g:LanguageClient_serverCommands = {
                \ 'javascript': ['/usr/bin/env', 'typescript-language-server', '--stdio'],
                \ 'javascriptreact': ['/usr/bin/env', 'typescript-language-server', '--stdio'],
                \ 'typescript': ['/usr/bin/env', 'typescript-language-server', '--stdio'],
                \ 'typescriptreact': ['/usr/bin/env', 'typescript-language-server', '--stdio'],
                \ 'python': ['/usr/bin/env', 'jedi-language-server'],
                \ 'haskell': ['/usr/bin/env', 'haskell-language-server-wrapper', '--lsp'],
                \ 'ruby': ['/usr/bin/env', 'solargraph', 'stdio'],
                \ 'json': ['/usr/bin/env', 'json-languageserver', '--stdio'],
                \ 'html': ['/usr/bin/env', 'html-languageserver', '--stdio'],
                \ 'scss': ['/usr/bin/env', 'css-languageserver', '--stdio'],
                \ 'css': ['/usr/bin/env', 'css-languageserver', '--stdio'],
                \ 'lua': ['sumneko-lua-language-server'],
                \ 'vim': ['/usr/bin/env', 'vim-language-server', '--stdio'],
                \ 'markdown': ['/usr/bin/env', 'citation-langserver'],
                \ 'bash': ['/usr/bin/env', 'bash-language-server', 'start'],
                \ 'sh': ['/usr/bin/env', 'bash-language-server', 'start'],
                \}
            endif
          " }}}
          " ALE For Linting: {{{
            call minpac#add('dense-analysis/ale')
            let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
            let g:ale_cursor_detail = 0
            " Always use preview for hover (only used w/ JS languages):
            " let g:ale_hover_to_preview = 1
            " We let our LSP client handle LSPs:
            let g:ale_disable_lsp = 1
            let g:ale_fix_on_save = 0
            let g:ale_pattern_options = {
              \  '\.min.js$': {'ale_enabled': 0},
              \  'build/.*$': {'ale_enabled': 0},
              \}
            let g:ale_set_signs = 0 " Turn off sign column
          " }}}
        endif
      " }}}
      " Writing: {{{
        call minpac#add('godlygeek/tabular') " :Tabular /| to auto-align tables (also :TableFormat in markdown)
        call minpac#add('reedes/vim-textobj-sentence', { 'type': 'opt' }) " Use as & is for selecting sentences; g) and g( for moving
        call minpac#add('reedes/vim-textobj-quote', { 'type': 'opt' }) " Makes aq & iq for smart quotes
        let g:textobj#quote#educate = 0
        " Initialize the plugin when it is dynamically loaded:
        augroup load-sentence
          autocmd!
          autocmd FileType markdown,text packadd vim-textobj-sentence
          autocmd FileType markdown,text packadd vim-textobj-quote
          autocmd User vim-textobj-sentence call textobj#sentence#init()
          autocmd User vim-textobj-quote call textobj#quote#init()
        augroup END
      " }}}
    else " Server-only plugins
      call minpac#add('sheerun/vim-polyglot')
      if !has('nvim')
        call minpac#add('https://gitlab.com/protesilaos/tempus-themes-vim.git')
      endif
    endif
endfunction

" Define user commands for updating/cleaning the plugins.
" Each of them calls PackInit() to load minpac and register
" the information of plugins, then performs the task.
command! PackUpdate call PackInit() | call minpac#update()
command! PackClean  call PackInit() | call minpac#clean()
command! PackStatus packadd minpac | call minpac#status()

packloadall

runtime vimrc-minimal

" Treesitter {{{
if has('nvim-0.5') && (g:dotfiles_mode ==# 'desktop')
  lua require('dotfiles.treesitter')
endif
" }}}
" Maps: {{{
  " Select whole file
  nnoremap <leader>vf ggVG
  " Clear currently highlighted regexp:
  nnoremap <silent> <leader>cr :let<C-u>let @/=""<CR>
  " Highlight a block and type "@" to run a macro on the block:
  xnoremap <silent> @ :<C-u>call visualat#execute_macro_over_visual_range()<CR>
  " <F4> makes:
  nnoremap <silent><buffer> <F4> :make!<CR>
  " Update fast folds:
  nmap zuz <Plug>(FastFoldUpdate)
" Navigate quickfix:
nnoremap <silent> ]q :cnext<CR>
nnoremap <silent> [q :cprevious<CR>
" Grep project:
function s:grep_or_qfgrep()
  if &buftype ==# 'quickfix'
    call QFGrep#grep_QuickFix(0) 
  else
    execute 'Grep ' . input('Grep/')
  endif 
endfunction
nnoremap <silent> <leader>/ <cmd>call <SID>grep_or_qfgrep()<CR>
" Calculator (not sure how this works):
inoremap <C-A> <C-O>yiW<End>=<C-R>=<C-R>0<CR>
" Shortcut to view current syntax highlighting group:
map <F10> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
  \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
  \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
" if has('nvim-0.5')
"   map <F10> :TSHighlightCapturesUnderCursor<CR>
" endif

" List Bindings: {{{
  nnoremap <silent> <leader>d :call dotfiles#lists#toggle('Location List', 'l')<CR>
  nnoremap <silent> <leader>q :call dotfiles#lists#toggle('Quickfix List', 'c')<CR>
"}}}

" Standard Fuzzy Bindings: {{{
  nmap <silent> <c-p> <Plug>(dotfiles-files)
  nmap <silent> <leader>F <Plug>(dotfiles-home-files)
  nmap <silent> <leader>a <Plug>(dotfiles-buffers)
  nmap <silent> <leader>A <Plug>(dotfiles-windows)
  nmap <silent> <leader>l <Plug>(dotfiles-lines)
  nmap <silent> <leader>? <Plug>(dotfiles-commands)
" }}}
" Standard LSP Bindings: {{{
  " As with Fuzzy bindings (above), we set all the LSP commands to Plug
  " bindings and then rebind them here to the keys we actually want to use:
  if (g:dotfiles_mode ==# 'desktop')
    nmap <silent> <F2>      <Plug>(dotfiles-rename)
    nmap <silent> <F5>      <Plug>(dotfiles-commands)
    vmap <silent> ga        <Plug>(dotfiles-codeaction-selected)
    nmap <silent> ga        <Plug>(dotfiles-codeaction)
    nmap <silent> gl        <Plug>(dotfiles-codelens)
    nmap <silent> gd        <Plug>(dotfiles-definition)
    nmap <silent> gy        <Plug>(dotfiles-type-definition)
    nmap <silent> gi        <Plug>(dotfiles-implementation)
    nmap <silent> gr        <Plug>(dotfiles-references)
    nmap <silent> <leader>s <Plug>(dotfiles-document-symbols)
    nmap <silent> K         <Plug>(dotfiles-documentation)
    nmap <silent> [d        <Plug>(dotfiles-diagnostic-previous)
    nmap <silent> ]d        <Plug>(dotfiles-diagnostic-next)
  endif
" }}}
" }}}
" Theme: {{{
set background=dark
" Fancy Colors for Desktop Mode:
if g:dotfiles_mode ==# 'desktop'
if exists('+termguicolors')
  let &t_8f="\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b="\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
  " nvim-colorizer:
  if has('nvim')
    lua require'colorizer'.setup{'*',markdown={names=false},text={names=false}}
  endif
endif
endif
if has('nvim')
colorscheme wal
else
augroup tempus_color_additions
  autocmd!
  autocmd ColorScheme tempus_classic call dotfiles#colors#tempus_color_additions()
augroup END
colorscheme tempus_classic
endif
" }}}
" Other Settings: {{{
augroup dotfile-autocmds
  autocmd!
  " Close preview window:
  autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif
  " On opening a file, jump to the last known cursor position (see :h line())
  autocmd! BufReadPost *
        \ if line("'\"") > 1 && line("'\"") <= line("$") && &ft !~# 'commit' |
        \   exe "normal! g`\"" |
        \ endif 
augroup END
" Load Autocompletion: {{{
if g:dotfiles_mode ==# 'desktop'
  call dotfiles#autocomplete#init()
else
  call dotfiles#autocomplete#fzf#init()
endif
" }}}
" Update FASD For NeoVim: {{{
if has('nvim')
  function! s:fasd_update() abort
    if empty(&buftype)
      call jobstart(['fasd', '-A', expand('%:p')])
    endif
  endfunction
  augroup fasd
    autocmd!
    autocmd BufWinEnter,BufFilePost * call s:fasd_update()
  augroup END
endif
" }}}
" }}}
" Server Only Settings: {{{
if dotfiles_mode ==# 'server'
  " Minimal format for server mode:
  command! Format :exe "normal! gggqG<C-o>"
  augroup dotfiles-server-au
    autocmd!
    " Turn on CursorLine Highlighting on Insert:
    autocmd InsertEnter,InsertLeave * set cul!
  augroup END
endif
" }}}
" # vim:foldmethod=marker
