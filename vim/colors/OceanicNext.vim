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
" {{{ Basics
hi Bold cterm='bold'
hi Debug ctermfg=01
hi Directory ctermfg=04
hi ErrorMsg ctermfg=01 ctermbg=NONE
hi Exception ctermfg=01
hi FoldColumn ctermfg=04 ctermbg=NONE
hi Folded ctermfg=08 ctermbg=10 cterm='italic'
hi IncSearch ctermfg=10 ctermbg=09 cterm=NONE
hi Italic cterm='italic'

hi Macro ctermfg=01
hi MatchParen ctermfg=07 ctermbg=08
hi ModeMsg ctermfg=02
hi MoreMsg ctermfg=02
hi Question ctermfg=04
hi Search ctermfg=08 ctermbg=03
hi SpecialKey ctermfg=08
hi TooLong ctermfg=01
hi Underlined ctermfg=01
hi Visual ctermbg=11
hi VisualNOS ctermfg=01
hi WarningMsg ctermfg=01
hi WildMenu ctermfg=15 ctermbg=04
hi Title ctermfg=04
hi Conceal ctermfg=04 ctermbg=NONE
hi Cursor ctermfg=00 ctermbg=07
hi NonText ctermfg=08
hi Normal ctermfg=07 ctermbg=NONE
hi EndOfBuffer ctermfg=07 ctermbg=NONE
hi LineNr ctermfg=08 ctermbg=NONE
hi SignColumn ctermfg=00 ctermbg=NONE
hi StatusLine ctermfg=10 ctermbg=08 cterm=NONE
hi StatusLineNC ctermfg=08 ctermbg=10 cterm=NONE
hi VertSplit ctermfg=00 ctermbg=11
hi ColorColumn ctermbg=10
hi CursorColumn ctermbg=11
hi CursorLine ctermbg=10 cterm=NONE
hi CursorLineNR ctermfg=00 ctermbg=NONE
hi CursorLineNr ctermfg=08 ctermbg=10
hi PMenu ctermfg=12 ctermbg=10
hi PMenuSel ctermfg=15 ctermbg=04
hi PmenuSbar ctermbg=11
hi PmenuThumb ctermbg=07
hi TabLine ctermfg=08 ctermbg=10
hi TabLineFill ctermfg=08 ctermbg=10
hi TabLineSel ctermfg=02 ctermbg=10
hi helpExample ctermfg=03
hi helpCommand ctermfg=03
" }}}
" Syntax Highlighting {{{
hi Boolean ctermfg=09
hi Character ctermfg=01
hi Comment ctermfg=08 cterm='italic'
hi Conditional ctermfg=05
hi Constant ctermfg=09
hi Define ctermfg=05
hi Delimiter ctermfg=14
hi Float ctermfg=09
hi Function ctermfg=04
hi Identifier ctermfg=06
hi Include ctermfg=04
hi Keyword ctermfg=05
hi Label ctermfg=03
hi Number ctermfg=09
hi Operator ctermfg=07
hi PreProc ctermfg=03
hi Repeat ctermfg=03
hi Special ctermfg=06
hi SpecialChar ctermfg=14
hi Statement ctermfg=01
hi StorageClass ctermfg=03
hi String ctermfg=02
hi Structure ctermfg=05
hi Tag ctermfg=03
hi Todo ctermfg=03 ctermbg=10
hi Type ctermfg=03
hi Typedef ctermfg=03
hi Noise ctermfg=06
" }}}
" Spelling & Diffing {{{
hi SpellBad ctermbg=11 cterm='undercurl'
hi SpellLocal cterm='undercurl'
hi SpellCap ctermbg=10 cterm='undercurl'
hi SpellRare cterm='undercurl'

hi DiffAdd ctermfg=02 ctermbg=10 cterm='bold'
hi DiffChange ctermfg=08 ctermbg=10
hi DiffDelete ctermfg=01 ctermbg=10
hi DiffText ctermfg=04 ctermbg=10
hi DiffAdded ctermfg=15 ctermbg=02 cterm='bold'
hi DiffFile ctermfg=01 ctermbg=NONE
hi DiffNewFile ctermfg=02 ctermbg=NONE
hi DiffLine ctermfg=04 ctermbg=NONE
hi DiffRemoved ctermfg=15 ctermbg=01 cterm='bold'
" }}}
" Languages {{{
hi cssColor ctermfg=06
hi cssBraces ctermfg=07
hi cssClassName ctermfg=05

hi gitCommitOverflow ctermfg=01
hi gitCommitSummary ctermfg=02

hi htmlBold ctermfg=03
hi htmlItalic ctermfg=05
hi htmlTag ctermfg=06
hi htmlEndTag ctermfg=06
hi htmlArg ctermfg=03
hi htmlTagName ctermfg=07

hi jsGlobalObjects ctermfg=09
hi jsNumber ctermfg=09
hi jsBraces ctermfg=07
hi jsFuncCall ctermfg=04
hi jsStorageClass ctermfg=05

hi markdownCode ctermfg=02
hi markdownCodeBlock ctermfg=02
hi markdownHeadingDelimiter ctermfg=04
hi markdownItalic ctermfg=05 cterm='italic'
hi markdownBold ctermfg=03 cterm='bold'
hi markdownCodeDelimiter ctermfg=14 cterm='italic'
hi markdownError ctermfg=07 ctermbg=10

hi pythonRepeat ctermfg=05
hi pythonOperator ctermfg=05

hi rubyConstant ctermfg=03
hi rubySymbol ctermfg=02
hi rubyAttribute ctermfg=04
hi rubyInterpolation ctermfg=02
hi rubyInterpolationDelimiter ctermbg=14
hi rubyStringDelimiter ctermfg=02
hi rubyRegexp ctermfg=06

hi sassidChar ctermfg=01
hi sassClassChar ctermfg=09
hi sassInclude ctermfg=05
hi sassMixing ctermfg=05
hi sassMixinName ctermfg=04

hi xmlTag ctermfg=06
hi xmlTagName ctermfg=07
hi xmlEndTag ctermfg=06
"}}}
" Plugins {{{
hi ALEErrorSign ctermfg=01 ctermbg=NONE cterm='bold'
hi ALEWarningSign ctermfg=03 ctermbg=NONE cterm='bold'
hi ALEInfoSign ctermfg=15 ctermbg=NONE cterm='bold'
" }}}
" Unused Tags {{{

" hi Error ctermfg=01 ctermbg=NONE cterm='undercurl'
" hi NvimInternalError ctermfg=01 ctermbg=NONE
" hi NvimInternalError ctermfg=01 ctermbg=NONE

" hi NeomakeErrorSign ctermfg=01 ctermbg=NONE
" hi NeomakeWarningSign ctermfg=03 ctermbg=NONE
" hi NeomakeInfoSign ctermfg=15 ctermbg=NONE
" hi NeomakeError ctermfg=01 cterm='undercurl'
" hi NeomakeWarning ctermfg=01 cterm='undercurl'

" hi NERDTreeExecFile ctermfg=07
" hi NERDTreeDirSlash ctermfg=04
" hi NERDTreeOpenable ctermfg=04
" hi NERDTreeFile ctermbg=NONE
" hi NERDTreeFlags ctermfg=04

" hi phpComparison ctermfg=07
" hi phpParent ctermfg=07
" hi phpMemberSelector ctermfg=07
" hi vimfilerLeaf ctermfg=07
" hi vimfilerNormalFile ctermfg=07 ctermbg=NONE
" hi vimfilerOpenedFile ctermfg=04
" hi vimfilerClosedFile ctermfg=04

" hi GitGutterAdd ctermfg=02 ctermbg=NONE cterm='bold'
" hi GitGutterChange ctermfg=04 ctermbg=NONE cterm='bold'
" hi GitGutterDelete ctermfg=01 ctermbg=NONE cterm='bold'
" hi GitGutterChangeDelete ctermfg=05 ctermbg=NONE cterm='bold'

" hi SignifySignAdd ctermfg=02 ctermbg=NONE cterm='bold'
" hi SignifySignChange ctermfg=04 ctermbg=NONE cterm='bold'
" hi SignifySignDelete ctermfg=01 ctermbg=NONE cterm='bold'
" hi SignifySignChangeDelete ctermfg=05 ctermbg=NONE cterm='bold'
" hi SignifySignDeleteFirstLine ctermbg=01 ctermbg=NONE cterm='bold'

" hi csClass ctermfg=03
" hi csAttribute ctermfg=03
" hi csModifier ctermfg=05
" hi csType ctermfg=01
" hi csUnspecifiedStatement ctermfg=04
" hi csContextualStatement ctermfg=05
" hi csNewDecleration ctermfg=01
" hi cOperator ctermfg=06
" hi cPreCondit ctermfg=05

" }}}
