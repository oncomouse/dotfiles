" Syntax:
" Make sure .md files are read as Markdown:
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
" Load Indent Guides for Python Files:
autocmd BufNewFile,BufReadPost *.py IndentGuidesEnable

" Use vim-jsx-improved instead:
let g:polyglot_disabled = ['jsx']
