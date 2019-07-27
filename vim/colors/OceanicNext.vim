" ===============================================================
" OceanicNext
" Author: Mike Hartington
" ===============================================================

" {{{ Setup
  set background=dark
  hi clear
  if exists("syntax_on")
    syntax reset
  endif
  let g:colors_name="OceanicNext"
" }}}
" {{{ Italics
  let g:oceanic_next_terminal_italic = get(g:, 'oceanic_next_terminal_italic', 0)
  let s:italic = ""
  if g:oceanic_next_terminal_italic == 1
    let s:italic = "italic"
  endif
"}}}
" {{{ Bold
  let g:oceanic_next_terminal_bold = get(g:, 'oceanic_next_terminal_bold', 0)
  let s:bold = ""
  if g:oceanic_next_terminal_bold == 1
   let s:bold = "bold"
  endif
"}}}
" {{{ Colors
  let s:cterm00=['#1b2b34', '0']
  let s:cterm10=['#343d46', '10']
  let s:cterm11=['#4f5b66', '11']
  let s:cterm08=['#65737e', '8']
  let s:cterm12=['#a7adba', '12']
  let s:cterm07a=['#c0c5ce', '7']
  let s:cterm13=['#cdd3de', '13']
  let s:cterm07=['#d8dee9', '7']
  let s:cterm01=['#ec5f67', '1']
  let s:cterm09=['#f99157', '9']
  let s:cterm03=['#fac863', '3']
  let s:cterm02=['#99c794', '2']
  let s:cterm06=['#62b3b2', '6']
  let s:cterm04=['#6699cc', '4']
  let s:cterm05=['#c594c5', '5']
  let s:cterm14=['#ab7967', '14']
  let s:cterm15=['#ffffff', '15']
  let s:none=['NONE', 'NONE']

" }}}
" {{{ Highlight function
function! <sid>hi(group, fg, bg, attr)
  " fg, bg, attr, attrsp
  if !empty(a:fg)
    exec "hi " . a:group . " guifg=" .  a:fg[0]
    exec "hi " . a:group . " ctermfg=" . a:fg[1]
  endif
  if !empty(a:bg)
    exec "hi " . a:group . " guibg=" .  a:bg[0]
    exec "hi " . a:group . " ctermbg=" . a:bg[1]
  endif
  if a:attr != ""
    exec "hi " . a:group . " gui=" .   a:attr
    exec "hi " . a:group . " cterm=" . a:attr
  endif
endfunction
" }}}
" {{{ call <sid>:hi(group, fg, bg, gui)
call <sid>hi('Bold',                       '',       '',       'bold')
call <sid>hi('Debug',                      s:cterm01, '',       '')
call <sid>hi('Directory',                  s:cterm04, '',       '')
call <sid>hi('ErrorMsg',                   s:cterm01, s:none, '')
call <sid>hi('Exception',                  s:cterm01, '',       '')
call <sid>hi('FoldColumn',                 s:cterm04, s:none, '')
call <sid>hi('Folded',                     s:cterm08, s:cterm00, s:italic)
call <sid>hi('IncSearch',                  s:cterm10, s:cterm09, 'NONE')
call <sid>hi('Italic',                     '',       '',       s:italic)

call <sid>hi('Macro',                      s:cterm01, '',       '')
call <sid>hi('MatchParen',                 s:cterm07a, s:cterm08, '')
call <sid>hi('ModeMsg',                    s:cterm02, '',       '')
call <sid>hi('MoreMsg',                    s:cterm02, '',       '')
call <sid>hi('Question',                   s:cterm04, '',       '')
call <sid>hi('Search',                     s:cterm08, s:cterm03, '')
call <sid>hi('SpecialKey',                 s:cterm08, '',       '')
call <sid>hi('TooLong',                    s:cterm01, '',       '')
call <sid>hi('Underlined',                 s:cterm01, '',       '')
call <sid>hi('Visual',                     '',       s:cterm11, '')
call <sid>hi('VisualNOS',                  s:cterm01, '',       '')
call <sid>hi('WarningMsg',                 s:cterm01, '',       '')
call <sid>hi('WildMenu',                   s:cterm15, s:cterm04, '')
call <sid>hi('Title',                      s:cterm04, '',       '')
call <sid>hi('Conceal',                    s:cterm04, s:none, '')
call <sid>hi('Cursor',                     s:cterm00, s:cterm07a, '')
call <sid>hi('NonText',                    s:cterm08, '',       '')
call <sid>hi('Normal',                     s:cterm07a, s:none, '')
call <sid>hi('EndOfBuffer',                s:cterm07a, s:none, '')
call <sid>hi('LineNr',                     s:cterm08, s:none, '')
call <sid>hi('SignColumn',                 s:cterm00, s:none, '')
call <sid>hi('StatusLine',                 s:cterm10, s:cterm08, 'NONE')
call <sid>hi('StatusLineNC',               s:cterm08, s:cterm10, 'NONE')
call <sid>hi('VertSplit',                  s:cterm00, s:cterm11, '')
call <sid>hi('ColorColumn',                '',       s:cterm00, '')
call <sid>hi('CursorColumn',               '',       s:cterm10, '')
call <sid>hi('CursorLine',                 '',       s:cterm10, 'NONE')
call <sid>hi('CursorLineNR',               s:cterm00, s:none, '')
call <sid>hi('CursorLineNr',               s:cterm08, s:cterm10, '')
call <sid>hi('PMenu',                      s:cterm12, s:cterm10, '')
call <sid>hi('PMenuSel',                   s:cterm15, s:cterm04, '')
call <sid>hi('PmenuSbar',                  '',       s:cterm11, '')
call <sid>hi('PmenuThumb',                 '',       s:cterm07, '')
call <sid>hi('TabLine',                    s:cterm08, s:cterm10, '')
call <sid>hi('TabLineFill',                s:cterm08, s:cterm10, '')
call <sid>hi('TabLineSel',                 s:cterm02, s:cterm10, '')
call <sid>hi('helpExample',                s:cterm03, '',       '')
call <sid>hi('helpCommand',                s:cterm03, '',       '')

" Standard syntax highlighting
call <sid>hi('Boolean',                    s:cterm09, '',       '')
call <sid>hi('Character',                  s:cterm01, '',       '')
call <sid>hi('Comment',                    s:cterm08, '',       s:italic)
call <sid>hi('Conditional',                s:cterm05, '',       '')
call <sid>hi('Constant',                   s:cterm09, '',       '')
call <sid>hi('Define',                     s:cterm05, '',       '')
call <sid>hi('Delimiter',                  s:cterm14, '',       '')
call <sid>hi('Float',                      s:cterm09, '',       '')
call <sid>hi('Function',                   s:cterm04, '',       '')
call <sid>hi('Identifier',                 s:cterm06, '',       '')
call <sid>hi('Include',                    s:cterm04, '',       '')
call <sid>hi('Keyword',                    s:cterm05, '',       '')
call <sid>hi('Label',                      s:cterm03, '',       '')
call <sid>hi('Number',                     s:cterm09, '',       '')
call <sid>hi('Operator',                   s:cterm07a, '',       '')
call <sid>hi('PreProc',                    s:cterm03, '',       '')
call <sid>hi('Repeat',                     s:cterm03, '',       '')
call <sid>hi('Special',                    s:cterm06, '',       '')
call <sid>hi('SpecialChar',                s:cterm14, '',       '')
call <sid>hi('Statement',                  s:cterm01, '',       '')
call <sid>hi('StorageClass',               s:cterm03, '',       '')
call <sid>hi('String',                     s:cterm02, '',       '')
call <sid>hi('Structure',                  s:cterm05, '',       '')
call <sid>hi('Tag',                        s:cterm03, '',       '')
call <sid>hi('Todo',                       s:cterm03, s:cterm10, '')
call <sid>hi('Type',                       s:cterm03, '',       '')
call <sid>hi('Typedef',                    s:cterm03, '',       '')
call <sid>hi('Noise',                      s:cterm06,  '',       '')

call <sid>hi('SpellBad',                   '',       s:cterm11,'undercurl')
call <sid>hi('SpellLocal',                 '',       '',       'undercurl')
call <sid>hi('SpellCap',                   '',       s:cterm10,'undercurl')
call <sid>hi('SpellRare',                  '',       '',       'undercurl')

call <sid>hi('csClass',                    s:cterm03, '',       '')
call <sid>hi('csAttribute',                s:cterm03, '',       '')
call <sid>hi('csModifier',                 s:cterm05, '',       '')
call <sid>hi('csType',                     s:cterm01, '',       '')
call <sid>hi('csUnspecifiedStatement',     s:cterm04, '',       '')
call <sid>hi('csContextualStatement',      s:cterm05, '',       '')
call <sid>hi('csNewDecleration',           s:cterm01, '',       '')
call <sid>hi('cOperator',                  s:cterm06, '',       '')
call <sid>hi('cPreCondit',                 s:cterm05, '',       '')

call <sid>hi('cssColor',                   s:cterm06, '',       '')
call <sid>hi('cssBraces',                  s:cterm07a, '',       '')
call <sid>hi('cssClassName',               s:cterm05, '',       '')


call <sid>hi('DiffAdd',                    s:cterm02, s:cterm10, 'bold')
call <sid>hi('DiffChange',                 s:cterm08, s:cterm10, '')
call <sid>hi('DiffDelete',                 s:cterm01, s:cterm10, '')
call <sid>hi('DiffText',                   s:cterm04, s:cterm10, '')
call <sid>hi('DiffAdded',                  s:cterm15, s:cterm02, 'bold')
call <sid>hi('DiffFile',                   s:cterm01, s:none, '')
call <sid>hi('DiffNewFile',                s:cterm02, s:none, '')
call <sid>hi('DiffLine',                   s:cterm04, s:none, '')
call <sid>hi('DiffRemoved',                s:cterm15, s:cterm01, 'bold')

" call <sid>hi('Error',                      s:cterm01, s:none,   'undercurl')
" call <sid>hi('NvimInternalError',          s:cterm01, s:none,   '')
" call <sid>hi('NvimInternalError',          s:cterm01, s:none,   '')
call <sid>hi('gitCommitOverflow',          s:cterm01, '',       '')
call <sid>hi('gitCommitSummary',           s:cterm02, '',       '')

call <sid>hi('htmlBold',                   s:cterm03, '',       '')
call <sid>hi('htmlItalic',                 s:cterm05, '',       '')
call <sid>hi('htmlTag',                    s:cterm06, '',       '')
call <sid>hi('htmlEndTag',                 s:cterm06, '',       '')
call <sid>hi('htmlArg',                    s:cterm03, '',       '')
call <sid>hi('htmlTagName',                s:cterm07, '',       '')

call <sid>hi('jsGlobalObjects',            s:cterm09, '',       '')
call <sid>hi('jsNumber',                   s:cterm09, '',       '')
call <sid>hi('javaScriptBraces',           s:cterm07a, '',      '')
call <sid>hi('jsFuncCall',                 s:cterm04, '',       '')
call <sid>hi('jsStorageClass',             s:cterm05, '',       '')

call <sid>hi('markdownCode',               s:cterm02, '',       '')
call <sid>hi('markdownCodeBlock',          s:cterm02, '',       '')
call <sid>hi('markdownHeadingDelimiter',   s:cterm04, '',       '')
call <sid>hi('markdownItalic',             s:cterm05, '',       s:italic)
call <sid>hi('markdownBold',               s:cterm03, '',       s:bold)
call <sid>hi('markdownCodeDelimiter',      s:cterm14, '',       s:italic)
call <sid>hi('markdownError',              s:cterm07a, s:cterm00, '')

call <sid>hi('NeomakeErrorSign',           s:cterm01, s:none, '')
call <sid>hi('NeomakeWarningSign',         s:cterm03, s:none, '')
call <sid>hi('NeomakeInfoSign',            s:cterm15, s:none, '')
call <sid>hi('NeomakeError',               s:cterm01, '',       'undercurl')
call <sid>hi('NeomakeWarning',             s:cterm01, '',       'undercurl')

call <sid>hi('ALEErrorSign',               s:cterm01, s:none, s:bold)
call <sid>hi('ALEWarningSign',             s:cterm03, s:none, s:bold)
call <sid>hi('ALEInfoSign',                s:cterm15, s:none, s:bold)

call <sid>hi('NERDTreeExecFile',           s:cterm07a, '',       '')
call <sid>hi('NERDTreeDirSlash',           s:cterm04, '',       '')
call <sid>hi('NERDTreeOpenable',           s:cterm04, '',       '')
call <sid>hi('NERDTreeFile',               '',       s:none,   '')
call <sid>hi('NERDTreeFlags',              s:cterm04, '',       '')

call <sid>hi('phpComparison',              s:cterm07a, '',       '')
call <sid>hi('phpParent',                  s:cterm07a, '',       '')
call <sid>hi('phpMemberSelector',          s:cterm07a, '',       '')

call <sid>hi('pythonRepeat',               s:cterm05, '',       '')
call <sid>hi('pythonOperator',             s:cterm05, '',       '')

call <sid>hi('rubyConstant',               s:cterm03, '',       '')
call <sid>hi('rubySymbol',                 s:cterm02, '',       '')
call <sid>hi('rubyAttribute',              s:cterm04, '',       '')
call <sid>hi('rubyInterpolation',          s:cterm02, '',       '')
call <sid>hi('rubyInterpolationDelimiter', s:cterm14, '',       '')
call <sid>hi('rubyStringDelimiter',        s:cterm02, '',       '')
call <sid>hi('rubyRegexp',                 s:cterm06, '',       '')

call <sid>hi('sassidChar',                 s:cterm01, '',       '')
call <sid>hi('sassClassChar',              s:cterm09, '',       '')
call <sid>hi('sassInclude',                s:cterm05, '',       '')
call <sid>hi('sassMixing',                 s:cterm05, '',       '')
call <sid>hi('sassMixinName',              s:cterm04, '',       '')

call <sid>hi('vimfilerLeaf',               s:cterm07a, '',       '')
call <sid>hi('vimfilerNormalFile',         s:cterm07a, s:none, '')
call <sid>hi('vimfilerOpenedFile',         s:cterm04, '',       '')
call <sid>hi('vimfilerClosedFile',         s:cterm04, '',       '')

call <sid>hi('GitGutterAdd',               s:cterm02, s:none, s:bold)
call <sid>hi('GitGutterChange',            s:cterm04, s:none, s:bold)
call <sid>hi('GitGutterDelete',            s:cterm01, s:none, s:bold)
call <sid>hi('GitGutterChangeDelete',      s:cterm05, s:none, s:bold)

call <sid>hi('SignifySignAdd',             s:cterm02, s:none, s:bold)
call <sid>hi('SignifySignChange',          s:cterm04, s:none, s:bold)
call <sid>hi('SignifySignDelete',          s:cterm01, s:none, s:bold)
call <sid>hi('SignifySignChangeDelete',    s:cterm05, s:none, s:bold)
call <sid>hi('SignifySignDeleteFirstLine', s:cterm01, s:none, s:bold)

call <sid>hi('xmlTag',                     s:cterm06, '',       '')
call <sid>hi('xmlTagName',                 s:cterm07a, '',       '')
call <sid>hi('xmlEndTag',                  s:cterm06, '',       '')
" }}}
