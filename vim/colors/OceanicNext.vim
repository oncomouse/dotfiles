" ===============================================================
" Adapted to only use cterm colors from:
"   https://github.com/mhartington/oceanic-next/
" ===============================================================
" {{{ Setup
  set background=dark
  hi clear
  if exists("syntax_on")
    syntax reset
  endif
  let g:colors_name="OceanicNext"
" }}}
" System Colors{{{
  let s:kitty_colors = eval(system('fish -c kitty-colors'))
  let s:i = 0
  while s:i < len(s:kitty_colors)
    execute 'let s:cterm'.printf('%02d', s:i).'=["'.printf('%02d', s:i)'", "'.s:kitty_colors[s:i].'"]'
    execute 'let g:terminal_color_'.s:i.'="'.s:kitty_colors[s:i].'"'
    let s:i += 1
  endwhile
  let s:ctermNONE = ['NONE', 'NONE']
" }}}
" Color Function {{{
  function! <sid>hi(group, fg, bg, attr)
    " fg, bg, attr, attrsp
    if !empty(a:fg)
      exec "hi " . a:group . " ctermfg=" . a:fg[0]
      exec "hi " . a:group . " guifg=" .  a:fg[1]
    endif
    if !empty(a:bg)
      exec "hi " . a:group . " ctermbg=" . a:bg[0]
      exec "hi " . a:group . " guibg=" .  a:bg[1]
    endif
    if a:attr != ""
      exec "hi " . a:group . " gui=" .   a:attr
      exec "hi " . a:group . " cterm=" . a:attr
    endif
  endfunction
" }}}
" Statusline {{{
  call <sid>hi('VisualMode',s:cterm10,s:cterm09,'bold')
  call <sid>hi('InsertMode',s:cterm10,s:cterm02,'bold')
  call <sid>hi('ReplaceMode',s:cterm10,s:cterm01,'bold')
  call <sid>hi('CommandMode',s:cterm10,s:cterm05,'bold')
  call <sid>hi('NormalMode',s:cterm10,s:cterm04,'bold')
  call <sid>hi('StatusLine',s:cterm10,s:ctermNONE,'NONE')
  call <sid>hi('StatusLineNC', s:cterm11,s:ctermNONE, 'NONE')
  call <sid>hi('User1', '', s:cterm08, 'bold')
  call <sid>hi('User2', '', s:cterm08, 'NONE')
  call <sid>hi('User3', s:cterm10, s:cterm03, 'NONE')
  call <sid>hi('User4', s:cterm10, s:cterm01, 'NONE')
  call <sid>hi('User5', s:cterm10, s:cterm04, 'NONE')
  call <sid>hi('TabLineFill', s:cterm11, s:ctermNONE, '')
  call <sid>hi('TabLineSel',s:cterm13,s:cterm11,'bold')
  call <sid>hi('TabLine', s:cterm08, s:cterm10, '')
" }}}
" Basics {{{
call <sid>hi('Bold','','','bold')
call <sid>hi('Debug',s:cterm01,'','')
call <sid>hi('Directory',s:cterm04,'','')
call <sid>hi('ErrorMsg',s:cterm01,s:ctermNONE,'')
call <sid>hi('Exception',s:cterm01,'','')
call <sid>hi('FoldColumn',s:cterm04,s:ctermNONE,'')
call <sid>hi('Folded',s:cterm08,s:cterm10,'italic')
call <sid>hi('IncSearch',s:cterm10,s:cterm09,'NONE')
call <sid>hi('Italic','','','italic')

call <sid>hi('Macro',s:cterm01,'','')
call <sid>hi('MatchParen',s:cterm07,s:cterm08,'')
call <sid>hi('ModeMsg',s:cterm02,'','')
call <sid>hi('MoreMsg',s:cterm02,'','')
call <sid>hi('Question',s:cterm04,'','')
call <sid>hi('Search',s:cterm08,s:cterm03,'')
call <sid>hi('SpecialKey',s:cterm08,'','')
call <sid>hi('TooLong',s:cterm01,'','')
call <sid>hi('Underlined',s:cterm01,'','')
call <sid>hi('Visual','',s:cterm11,'')
call <sid>hi('VisualNOS',s:cterm01,'','')
call <sid>hi('WarningMsg',s:cterm01,'','')
call <sid>hi('WildMenu',s:cterm15,s:cterm04,'')
call <sid>hi('Title',s:cterm04,'','')
call <sid>hi('Conceal',s:cterm04,s:ctermNONE,'')
call <sid>hi('Cursor',s:cterm00,s:cterm07,'')
call <sid>hi('NonText',s:cterm08,'','')
call <sid>hi('Normal',s:cterm07,s:ctermNONE,'')
call <sid>hi('EndOfBuffer',s:cterm07,s:ctermNONE,'')
call <sid>hi('LineNr',s:cterm08,s:ctermNONE,'')
call <sid>hi('SignColumn',s:cterm00,s:ctermNONE,'')
call <sid>hi('VertSplit',s:cterm00,s:cterm11,'')
call <sid>hi('ColorColumn','',s:cterm10,'')
call <sid>hi('CursorColumn','',s:cterm11,'')
call <sid>hi('CursorLine','',s:cterm10,'NONE')
call <sid>hi('CursorLineNR',s:cterm00,s:ctermNONE,'')
call <sid>hi('CursorLineNr',s:cterm08,s:cterm10,'')
call <sid>hi('PMenu',s:cterm12,s:cterm10,'')
call <sid>hi('PMenuSel',s:cterm15,s:cterm04,'')
call <sid>hi('PmenuSbar','',s:cterm11,'')
call <sid>hi('PmenuThumb','',s:cterm07,'')
call <sid>hi('helpExample',s:cterm03,'','')
call <sid>hi('helpCommand',s:cterm03,'','')
" }}}
" Syntax Highlighting {{{
call <sid>hi('Boolean',s:cterm09,'','')
call <sid>hi('Character',s:cterm01,'','')
call <sid>hi('Comment',s:cterm08,'','italic')
call <sid>hi('Conditional',s:cterm05,'','')
call <sid>hi('Constant',s:cterm09,'','')
call <sid>hi('Define',s:cterm05,'','')
call <sid>hi('Delimiter',s:cterm14,'','')
call <sid>hi('Float',s:cterm09,'','')
call <sid>hi('Function',s:cterm04,'','')
call <sid>hi('Identifier',s:cterm06,'','bold')
call <sid>hi('Include',s:cterm04,'','')
call <sid>hi('Keyword',s:cterm05,'','')
call <sid>hi('Label',s:cterm03,'','')
call <sid>hi('Number',s:cterm09,'','')
call <sid>hi('Operator',s:cterm07,'','')
call <sid>hi('PreProc',s:cterm03,'','')
call <sid>hi('Repeat',s:cterm03,'','')
call <sid>hi('Special',s:cterm06,'','')
call <sid>hi('SpecialChar',s:cterm14,'','')
call <sid>hi('Statement',s:cterm01,'','NONE')
call <sid>hi('StorageClass',s:cterm03,'','')
call <sid>hi('String',s:cterm02,'','')
call <sid>hi('Structure',s:cterm05,'','')
call <sid>hi('Tag',s:cterm03,'','')
call <sid>hi('Todo',s:cterm03,s:cterm10,'')
call <sid>hi('Type',s:cterm03,'','')
call <sid>hi('Typedef',s:cterm03,'','')
call <sid>hi('Noise',s:cterm06,'','')
" }}}
" Spelling & Diffing {{{
call <sid>hi('SpellBad','',s:cterm11,'undercurl')
call <sid>hi('SpellLocal','','','undercurl')
call <sid>hi('SpellCap','',s:cterm10,'undercurl')
call <sid>hi('SpellRare','','','undercurl')

call <sid>hi('DiffAdd',s:cterm02,s:cterm10,'bold')
call <sid>hi('DiffChange',s:cterm08,s:cterm10,'')
call <sid>hi('DiffDelete',s:cterm01,s:cterm10,'')
call <sid>hi('DiffText',s:cterm04,s:cterm10,'')
call <sid>hi('DiffAdded',s:cterm15,s:cterm02,'bold')
call <sid>hi('DiffFile',s:cterm01,s:ctermNONE,'')
call <sid>hi('DiffNewFile',s:cterm02,s:ctermNONE,'')
call <sid>hi('DiffLine',s:cterm04,s:ctermNONE,'')
call <sid>hi('DiffRemoved',s:cterm15,s:cterm01,'bold')
" }}}
" Languages {{{
call <sid>hi('cssColor',s:cterm06,'','')
call <sid>hi('cssBraces',s:cterm07,'','')
call <sid>hi('cssClassName',s:cterm05,'','')

call <sid>hi('gitCommitOverflow',s:cterm01,'','')
call <sid>hi('gitCommitSummary',s:cterm02,'','')

call <sid>hi('htmlBold',s:cterm03,'','')
call <sid>hi('htmlItalic',s:cterm05,'','')
hi link htmlTag Noise
hi link htmlEndTag Noise
call <sid>hi('htmlArg',s:cterm09,'','')
call <sid>hi('htmlTagName',s:cterm01,'','')

call <sid>hi('jsGlobalObjects',s:cterm09,'','')
call <sid>hi('jsNumber',s:cterm09,'','')
call <sid>hi('jsBraces',s:cterm07,'','')
call <sid>hi('jsFuncCall',s:cterm04,'','')
call <sid>hi('jsStorageClass',s:cterm05,'','')
hi link jsxAttrib htmlArg
hi link jsxComponentName htmlTagName
hi link jsxEndComponentName htmlTagName

call <sid>hi('markdownCode',s:cterm02,'','')
call <sid>hi('markdownCodeBlock',s:cterm02,'','')
call <sid>hi('markdownHeadingDelimiter',s:cterm04,'','')
call <sid>hi('markdownItalic',s:cterm05,'','italic')
call <sid>hi('markdownBold',s:cterm03,'','bold')
call <sid>hi('markdownCodeDelimiter',s:cterm14,'','italic')
call <sid>hi('markdownError',s:cterm07,s:cterm10,'')

call <sid>hi('pythonRepeat',s:cterm05,'','')
call <sid>hi('pythonOperator',s:cterm05,'','')

call <sid>hi('rubyConstant',s:cterm03,'','')
call <sid>hi('rubySymbol',s:cterm02,'','')
call <sid>hi('rubyAttribute',s:cterm04,'','')
call <sid>hi('rubyInterpolation',s:cterm02,'','')
call <sid>hi('rubyInterpolationDelimiter','',s:cterm14,'')
call <sid>hi('rubyStringDelimiter',s:cterm02,'','')
call <sid>hi('rubyRegexp',s:cterm06,'','')

call <sid>hi('sassidChar',s:cterm01,'','')
call <sid>hi('sassClassChar',s:cterm09,'','')
call <sid>hi('sassInclude',s:cterm05,'','')
call <sid>hi('sassMixing',s:cterm05,'','')
call <sid>hi('sassMixinName',s:cterm04,'','')

call <sid>hi('xmlTag',s:cterm06,'','')
call <sid>hi('xmlTagName',s:cterm07,'','')
call <sid>hi('xmlEndTag',s:cterm06,'','')
"}}}
" Plugins {{{
call <sid>hi('ALEErrorSign',s:cterm01,s:ctermNONE,'bold')
call <sid>hi('ALEWarningSign',s:cterm03,s:ctermNONE,'bold')
call <sid>hi('ALEInfoSign',s:cterm15,s:ctermNONE,'bold')
" }}}
" Unused Tags {{{

" call <sid>hi('Error',s:cterm01,s:ctermNONE,'undercurl')
" call <sid>hi('NvimInternalError',s:cterm01,s:ctermNONE,'')
" call <sid>hi('NvimInternalError',s:cterm01,s:ctermNONE,'')

" call <sid>hi('NeomakeErrorSign',s:cterm01,s:ctermNONE,'')
" call <sid>hi('NeomakeWarningSign',s:cterm03,s:ctermNONE,'')
" call <sid>hi('NeomakeInfoSign',s:cterm15,s:ctermNONE,'')
" call <sid>hi('NeomakeError',s:cterm01,'','undercurl')
" call <sid>hi('NeomakeWarning',s:cterm01,'','undercurl')

" call <sid>hi('NERDTreeExecFile',s:cterm07,'','')
" call <sid>hi('NERDTreeDirSlash',s:cterm04,'','')
" call <sid>hi('NERDTreeOpenable',s:cterm04,'','')
" call <sid>hi('NERDTreeFile','',s:cterm:NONE,'','')
" call <sid>hi('NERDTreeFlags',s:cterm04,'','')

" call <sid>hi('phpComparison',s:cterm07,'','')
" call <sid>hi('phpParent',s:cterm07,'','')
" call <sid>hi('phpMemberSelector',s:cterm07,'','')
" call <sid>hi('vimfilerLeaf',s:cterm07,'','')
" call <sid>hi('vimfilerNormalFile',s:cterm07,s:ctermNONE,'')
" call <sid>hi('vimfilerOpenedFile',s:cterm04,'','')
" call <sid>hi('vimfilerClosedFile',s:cterm04,'','')

" call <sid>hi('GitGutterAdd',s:cterm02,s:ctermNONE,'bold')
" call <sid>hi('GitGutterChange',s:cterm04,s:ctermNONE,'bold')
" call <sid>hi('GitGutterDelete',s:cterm01,s:ctermNONE,'bold')
" call <sid>hi('GitGutterChangeDelete',s:cterm05,s:ctermNONE,'bold')

" call <sid>hi('SignifySignAdd',s:cterm02,s:ctermNONE,'bold')
" call <sid>hi('SignifySignChange',s:cterm04,s:ctermNONE,'bold')
" call <sid>hi('SignifySignDelete',s:cterm01,s:ctermNONE,'bold')
" call <sid>hi('SignifySignChangeDelete',s:cterm05,s:ctermNONE,'bold')
" call <sid>hi('SignifySignDeleteFirstLine',s:cterm01,s:ctermNONE,'bold')

" call <sid>hi('csClass',s:cterm03,'','')
" call <sid>hi('csAttribute',s:cterm03,'','')
" call <sid>hi('csModifier',s:cterm05,'','')
" call <sid>hi('csType',s:cterm01,'','')
" call <sid>hi('csUnspecifiedStatement',s:cterm04,'','')
" call <sid>hi('csContextualStatement',s:cterm05,'','')
" call <sid>hi('csNewDecleration',s:cterm01,'','')
" call <sid>hi('cOperator',s:cterm06,'','')
" call <sid>hi('cPreCondit',s:cterm05,'','')
" }}}
