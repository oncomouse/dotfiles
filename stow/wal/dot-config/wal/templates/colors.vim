" Name: Tempus Classic
" Description: Dark theme with warm hues (WCAG AA compliant)
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Meta: Created with the Tempus Themes Generator
" URL: https://gitlab.com/protesilaos/tempus-themes-generator

set background=dark
highlight! clear
if exists('syntax_on')
        syntax reset
endif
let g:colors_name = 'tempus_wal'

" General
" -------
if exists('g:tempus_enforce_background_color')
        highlight! Normal guibg={background} guifg={foreground} ctermbg=0 ctermfg=15
else
        " NOTE the ctermbg=none is for terminals with transparency
        highlight! Normal guibg={background} guifg={foreground} ctermbg=none ctermfg=15
endif

highlight! Visual guibg={foreground} guifg={background} ctermbg=15 ctermfg=0
highlight! Search gui=underline,bold guibg={color8} guifg={foreground} cterm=underline,bold ctermbg=8 ctermfg=3
highlight! IncSearch gui=underline,bold guibg={color7} guifg={background} term=none cterm=underline,bold ctermbg=7 ctermfg=0

highlight! VertSplit gui=none cterm=none
highlight! TabLine gui=none guibg={color8} guifg={color7} cterm=none ctermbg=8 ctermfg=7
highlight! TabLineSel gui=none guibg={color6} guifg={background} cterm=none ctermbg=6 ctermfg=0
highlight! TabLineFill gui=none cterm=none

highlight! Comment gui=italic guifg={color7} cterm=none ctermfg=7
highlight! Todo gui=bold guibg={color8} guifg={color11} cterm=bold ctermbg=8 ctermfg=11

highlight! Warning gui=none guibg={color3} guifg={background} cterm=none ctermbg=3 ctermfg=0
highlight! WarningMsg gui=none guibg={color3} guifg={background} cterm=none ctermbg=3 ctermfg=0
highlight! Error gui=none guibg={color1} guifg={background} cterm=none ctermbg=1 ctermfg=0
highlight! ErrorMsg gui=none guibg={color1} guifg={background} cterm=none ctermbg=1 ctermfg=0

highlight! MatchParen gui=underline,bold guibg={color8} guifg={color7} cterm=underline,bold ctermbg=8 ctermfg=7

highlight! ToolbarLine guibg={color7} guifg={background} term=none ctermbg=7 ctermfg=0
highlight! ToolbarButton gui=bold guibg={color7} guifg={background} term=none cterm=bold ctermbg=7 ctermfg=0

highlight! WildMenu guibg={background} guifg={foreground} term=standout ctermbg=0 ctermfg=15

highlight! Terminal guibg={background} guifg={foreground} term=none ctermbg=none ctermfg=15

" Constructs
" ----------
highlight! Constant guifg={color4} ctermfg=4
highlight! Number guifg={color4} ctermfg=4
highlight! Float guifg={color4} ctermfg=4
highlight! String guifg={color12} ctermfg=12

highlight! Function guifg={color5} ctermfg=5
highlight! Identifier guifg={color13} term=none ctermfg=13
highlight! Label guifg={color5} ctermfg=5
highlight! Tag guifg={color5} ctermfg=5
highlight! Keyword gui=bold guifg={color13} gui=bold ctermfg=13

highlight! Character gui=bold guifg={color14} cterm=bold ctermfg=14

highlight! Type gui=none,bold guifg={color6} term=none cterm=none,bold ctermfg=6
highlight! Boolean guifg={color6} ctermfg=6
highlight! StorageClass guifg={color6} ctermfg=6
highlight! Structure guifg={color6} ctermfg=6
highlight! Typedef gui=bold guifg={color14} cterm=bold ctermfg=14

highlight! Conditional gui=bold guifg={color2} cterm=bold ctermfg=2
highlight! Statement gui=none guifg={color10} cterm=none ctermfg=10
highlight! Repeat gui=bold guifg={color10} cterm=bold ctermfg=10
highlight! Operator gui=bold guifg={foreground} cterm=bold ctermfg=15
highlight! Exception gui=bold guifg={color1} cterm=bold ctermfg=1

highlight! Preproc gui=none guifg={color9} term=none cterm=none ctermfg=9
highlight! PreCondit gui=bold guifg={color9} cterm=bold ctermfg=9
highlight! Macro gui=bold guifg={color9} cterm=bold ctermfg=9
highlight! Include guifg={color9} ctermfg=9
highlight! Define guifg={color9} ctermfg=9

highlight! Title gui=bold guibg={background} guifg={color6} cterm=bold ctermbg=none ctermfg=6

highlight! Delimeter gui=bold guifg={color5} cterm=bold ctermfg=5
highlight! Delimiter gui=bold guifg={color5} cterm=bold ctermfg=5
highlight! SpecialComment gui=bold guifg={color5} cterm=bold ctermfg=5

highlight! Debug guifg={color13} ctermfg=13

" Other
" -----
highlight! LineNr guibg={color8} guifg={color7} term=none ctermbg=8 ctermfg=7
highlight! Cursor guibg={foreground} guifg={background} ctermbg=15 ctermfg=0
highlight! CursorLine gui=none guibg={color8} term=none cterm=none ctermbg=8
highlight! CursorColumn gui=none guibg={color8} term=none cterm=none ctermbg=8
highlight! CursorLineNr gui=bold guibg={color7} guifg={background} cterm=bold ctermbg=7 ctermfg=0
highlight! ColorColumn guibg={color8} guifg={foreground} term=none ctermbg=8 ctermfg=15
highlight! SignColumn guibg={color8} guifg={color7} term=none ctermbg=8 ctermfg=7

highlight! Folded guibg={color8} guifg={color7} ctermbg=8 ctermfg=7
highlight! FoldColumn guibg={color8} guifg={color7} ctermbg=8 ctermfg=7

highlight! Special gui=bold guifg={color11} term=none cterm=bold ctermfg=11
highlight! SpecialKey gui=none guibg={color8} guifg={color7} cterm=none ctermbg=8 ctermfg=7
highlight! SpecialChar gui=bold guifg={color11} cterm=bold ctermfg=11
highlight! NonText gui=none guibg={color8} guifg={color7} cterm=none ctermbg=8 ctermfg=7
highlight! EndOfBuffer gui=bold guifg={color7} cterm=bold ctermfg=7

highlight! Directory gui=none guifg={color2} term=none cterm=none ctermfg=2
highlight! Question gui=bold guifg={color11} cterm=bold ctermfg=11
highlight! MoreMsg guifg={color10} ctermfg=10
highlight! ModeMsg gui=bold guifg={color2} cterm=bold ctermfg=2

highlight! VimOption guifg={color5} ctermfg=5
highlight! VimGroup guifg={color5} ctermfg=5

highlight! Underlined gui=underline guifg={foreground} cterm=underline ctermfg=15
highlight! Ignore guibg={color8} guifg={color7} ctermbg=8 ctermfg=7
" hi Conceal guibg={color7} guifg={color8} ctermbg=7 ctermfg=8
highlight! Conceal guifg=Grey30 ctermfg=239 guibg=NONE ctermbg=NONE cterm=nocombine gui=nocombine

highlight! SpellBad guibg={color1} guifg={background} ctermbg=1 ctermfg=0
highlight! SpellCap guibg={color3} guifg={background} ctermbg=3 ctermfg=0
highlight! SpellRare guibg={color13} guifg={background} ctermbg=13 ctermfg=0
highlight! SpellLocal guibg={color14} guifg={background} ctermbg=14 ctermfg=0

highlight! Pmenu gui=italic guibg={color8} guifg={foreground} cterm=none ctermbg=8 ctermfg=15
highlight! PmenuSel gui=none,bold guibg={color7} guifg={background} cterm=none,bold ctermbg=7 ctermfg=0
highlight! PmenuSbar guibg={color8} ctermbg=8
highlight! PmenuThumb guibg={color7} ctermbg=7

highlight! RedSign ctermfg=5 guifg={color5} ctermbg=8 guibg={color8} cterm=NONE gui=NONE
highlight! YellowSign ctermfg=3 guifg={color3} ctermbg=8 guibg={color8} cterm=NONE gui=NONE
highlight! BlueSign ctermfg=4 guifg={color4} ctermbg=8 guibg={color8} cterm=NONE gui=NONE
highlight! GreenSign ctermfg=2 guifg={color2} ctermbg=8 guibg={color8} cterm=NONE gui=NONE
highlight! Orange ctermfg=1 guifg={color1} ctermbg=NONE guibg=NONE cterm=NONE gui=NONE
highlight! Green ctermfg=2 guifg={color2} ctermbg=NONE guibg=NONE cterm=NONE gui=NONE
highlight! Yellow ctermfg=3 guifg={color3} ctermbg=NONE guibg=NONE cterm=NONE gui=NONE
highlight! Blue ctermfg=4 guifg={color4} ctermbg=NONE guibg=NONE cterm=NONE gui=NONE
highlight! Red ctermfg=5 guifg={color5} ctermbg=NONE guibg=NONE cterm=NONE gui=NONE
highlight! Cyan ctermfg=6 guifg={color6} ctermbg=NONE guibg=NONE cterm=NONE gui=NONE
highlight! Grey ctermfg=7 guifg={color7} ctermbg=NONE guibg=NONE cterm=NONE gui=NONE
highlight! OrangeBright ctermfg=9 guifg={color9} ctermbg=NONE guibg=NONE cterm=NONE gui=NONE
highlight! GreenBright ctermfg=10 guifg={color10} ctermbg=NONE guibg=NONE cterm=NONE gui=NONE
highlight! YellowBright ctermfg=11 guifg={color11} ctermbg=NONE guibg=NONE cterm=NONE gui=NONE
highlight! BlueBright ctermfg=12 guifg={color12} ctermbg=NONE guibg=NONE cterm=NONE gui=NONE
highlight! RedBright ctermfg=13 guifg={color13} ctermbg=NONE guibg=NONE cterm=NONE gui=NONE
highlight! CyanBright ctermfg=14 guifg={color14} ctermbg=NONE guibg=NONE cterm=NONE gui=NONE
highlight! Fg ctermfg=15 guifg={color15} ctermbg=NONE guibg=NONE cterm=NONE gui=NONE
highlight! OrangeEmphasis ctermfg=1 guifg={color1} ctermbg=NONE guibg=NONE cterm=bold gui=bold
highlight! GreenEmphasis ctermfg=2 guifg={color2} ctermbg=NONE guibg=NONE cterm=bold gui=bold
highlight! YellowEmphasis ctermfg=3 guifg={color3} ctermbg=NONE guibg=NONE cterm=bold gui=bold
highlight! BlueEmphasis ctermfg=4 guifg={color4} ctermbg=NONE guibg=NONE cterm=bold gui=bold
highlight! RedEmphasis ctermfg=5 guifg={color5} ctermbg=NONE guibg=NONE cterm=bold gui=bold
highlight! CyanEmphasis ctermfg=6 guifg={color6} ctermbg=NONE guibg=NONE cterm=bold gui=bold
highlight! GreyEmphasis ctermfg=7 guifg={color7} ctermbg=NONE guibg=NONE cterm=bold gui=bold
highlight! OrangeBrightEmphasis ctermfg=9 guifg={color9} ctermbg=NONE guibg=NONE cterm=bold gui=bold
highlight! GreenBrightEmphasis ctermfg=10 guifg={color10} ctermbg=NONE guibg=NONE cterm=bold gui=bold
highlight! YellowBrightEmphasis ctermfg=11 guifg={color11} ctermbg=NONE guibg=NONE cterm=bold gui=bold
highlight! BlueBrightEmphasis ctermfg=12 guifg={color12} ctermbg=NONE guibg=NONE cterm=bold gui=bold
highlight! RedBrightEmphasis ctermfg=13 guifg={color13} ctermbg=NONE guibg=NONE cterm=bold gui=bold
highlight! CyanBrightEmphasis ctermfg=14 guifg={color14} ctermbg=NONE guibg=NONE cterm=bold gui=bold
highlight! FgEmphasis ctermfg=15 guifg={color15} ctermbg=NONE guibg=NONE cterm=bold gui=bold
highlight! OrangeItalic ctermfg=1 guifg={color1} ctermbg=NONE guibg=NONE cterm=italic gui=italic
highlight! GreenItalic ctermfg=2 guifg={color2} ctermbg=NONE guibg=NONE cterm=italic gui=italic
highlight! YellowItalic ctermfg=3 guifg={color3} ctermbg=NONE guibg=NONE cterm=italic gui=italic
highlight! BlueItalic ctermfg=4 guifg={color4} ctermbg=NONE guibg=NONE cterm=italic gui=italic
highlight! RedItalic ctermfg=5 guifg={color5} ctermbg=NONE guibg=NONE cterm=italic gui=italic
highlight! CyanItalic ctermfg=6 guifg={color6} ctermbg=NONE guibg=NONE cterm=italic gui=italic
highlight! GreyItalic ctermfg=7 guifg={color7} ctermbg=NONE guibg=NONE cterm=italic gui=italic
highlight! OrangeBrightItalic ctermfg=9 guifg={color9} ctermbg=NONE guibg=NONE cterm=italic gui=italic
highlight! GreenBrightItalic ctermfg=10 guifg={color10} ctermbg=NONE guibg=NONE cterm=italic gui=italic
highlight! YellowBrightItalic ctermfg=11 guifg={color11} ctermbg=NONE guibg=NONE cterm=italic gui=italic
highlight! BlueBrightItalic ctermfg=12 guifg={color12} ctermbg=NONE guibg=NONE cterm=italic gui=italic
highlight! RedBrightItalic ctermfg=13 guifg={color13} ctermbg=NONE guibg=NONE cterm=italic gui=italic
highlight! CyanBrightItalic ctermfg=14 guifg={color14} ctermbg=NONE guibg=NONE cterm=italic gui=italic
highlight! FgItalic ctermfg=15 guifg={color15} ctermbg=NONE guibg=NONE cterm=italic gui=italic

highlight! link ErrorFloat RedSign
highlight! link WarningFloat YellowSign
highlight! link InfoFloat BlueSign
highlight! link HintFloat GreenSign
highlight! link ErrorText RedSign
highlight! link WarningText YellowSign
highlight! link InfoText BlueSign
highlight! link HintText GreenSign
if has('nvim')
  highlight! link LspDiagnosticsFloatingError ErrorFloat
  highlight! link LspDiagnosticsFloatingWarning WarningFloat
  highlight! link LspDiagnosticsFloatingInformation InfoFloat
  highlight! link LspDiagnosticsFloatingHint HintFloat
  highlight! link LspDiagnosticsDefaultError ErrorText
  highlight! link LspDiagnosticsDefaultWarning WarningText
  highlight! link LspDiagnosticsDefaultInformation InfoText
  highlight! link LspDiagnosticsDefaultHint HintText
  highlight! link LspDiagnosticsVirtualTextError Grey
  highlight! link LspDiagnosticsVirtualTextWarning Grey
  highlight! link LspDiagnosticsVirtualTextInformation Grey
  highlight! link LspDiagnosticsVirtualTextHint Grey
  highlight! link LspDiagnosticsUnderlineError ErrorText
  highlight! link LspDiagnosticsUnderlineWarning WarningText
  highlight! link LspDiagnosticsUnderlineInformation InfoText
  highlight! link LspDiagnosticsUnderlineHint HintText
  highlight! link LspDiagnosticsSignError RedSign
  highlight! link LspDiagnosticsSignWarning YellowSign
  highlight! link LspDiagnosticsSignInformation BlueSign
  highlight! link LspDiagnosticsSignHint GreenSign
  highlight! link LspReferenceText CurrentWord
  highlight! link LspReferenceRead CurrentWord
  highlight! link LspReferenceWrite CurrentWord
  highlight! link TermCursor Cursor
  highlight! link healthError Red
  highlight! link healthSuccess Green
  highlight! link healthWarning Yellow
endif
" nvim-treesitter
  highlight! link TSAnnotation Blue
  highlight! link TSAttribute Blue
  highlight! link TSBoolean BlueBright
  highlight! link TSCharacter Yellow
  highlight! link TSComment Grey
  highlight! link TSConditional Red
  highlight! link TSConstBuiltin Orange
  highlight! link TSConstMacro Orange
  highlight! link TSConstant Orange
  highlight! link TSConstructor Fg
  highlight! link TSError ErrorText
  highlight! link TSException Red
  highlight! link TSField Green
  highlight! link TSFloat BlueBright
  highlight! link TSFuncBuiltin OrangeBright
  highlight! link TSFuncMacro OrangeBright
  highlight! link TSFunction OrangeBright
  highlight! link TSInclude OrangeBright
  highlight! link TSKeyword CyanEmphasis
  highlight! link TSKeywordFunction Red
  highlight! link TSLabel Red
  highlight! link TSMethod Green
  highlight! link TSNamespace BlueBright
  highlight! link TSNumber BlueBright
  highlight! link TSOperator Red
  highlight! link TSParameter Orange
  highlight! link TSParameterReference Orange
  highlight! link TSProperty Green
  highlight! link TSPunctBracket Fg
  highlight! link TSPunctDelimiter Grey
  highlight! link TSPunctSpecial Fg
  highlight! link TSRepeat Red
  highlight! link TSString Yellow
  highlight! link TSStringEscape Green
  highlight! link TSStringRegex Green
  highlight! link TSStructure Blue
  highlight! link TSTag Blue
  highlight! link TSTagDelimiter Red
  highlight! link TSText Green
  highlight! TSEmphasis ctermfg=NONE ctermbg=NONE guifg=NONE guibg=NONE cterm=bold gui=bold
  highlight! TSUnderline ctermfg=NONE ctermbg=NONE guifg=NONE guibg=NONE cterm=bold gui=underline
  highlight! link TSType Blue
  highlight! link TSTypeBuiltin Blue
  highlight! link TSURI markdownUrl
  highlight! link TSVariable Fg
  highlight! link TSVariableBuiltin Orange
" Terminal
" --------

if exists('*term_setansicolors')
        let g:terminal_ansi_colors = [
                                \ '{background}',
                                \ '{color1}',
                                \ '{color2}',
                                \ '{color3}',
                                \ '{color4}',
                                \ '{color5}',
                                \ '{color6}',
                                \ '{color7}',
                                \ '{color8}',
                                \ '{color9}',
                                \ '{color10}',
                                \ '{color11}',
                                \ '{color12}',
                                \ '{color13}',
                                \ '{color14}',
                                \ '{foreground}'
                                \ ]
endif
if has('nvim')
        let g:terminal_color_0 = '{background}'
        let g:terminal_color_1 = '{color1}'
        let g:terminal_color_2 = '{color2}'
        let g:terminal_color_3 = '{color3}'
        let g:terminal_color_4 = '{color4}'
        let g:terminal_color_5 = '{color5}'
        let g:terminal_color_6 = '{color6}'
        let g:terminal_color_7 = '{color7}'
        let g:terminal_color_8 = '{color8}'
        let g:terminal_color_9 = '{color9}'
        let g:terminal_color_10 = '{color10}'
        let g:terminal_color_11 = '{color11}'
        let g:terminal_color_12 = '{color12}'
        let g:terminal_color_13 = '{color13}'
        let g:terminal_color_14 = '{color14}'
        let g:terminal_color_15 = '{foreground}'
endif

" Diffs
" -----
highlight! DiffAdd gui=bold guibg={color2} guifg={background} cterm=bold ctermbg=10 ctermfg=0
highlight! DiffDelete gui=none guibg={color1} guifg={background} cterm=none ctermbg=9 ctermfg=0
highlight! DiffChange gui=bold guibg={color8} guifg={color7} cterm=bold ctermbg=8 ctermfg=7
highlight! DiffText gui=bold guibg={color8} guifg={color9} cterm=bold ctermbg=8 ctermfg=1

highlight! diffAdded guifg={color2} ctermfg=2
highlight! diffRemoved guifg={color1} ctermfg=1
highlight! diffNewFile gui=none guifg={color4} ctermfg=4
highlight! diffFile gui=none guifg={color3} cterm=none ctermfg=3

highlight! GitGutterAdd guibg={color8} guifg={color2} ctermbg=8 ctermfg=2
highlight! GitGutterChange gui=bold guibg={color8} guifg={color7} cterm=bold ctermbg=8 ctermfg=7
highlight! GitGutterDelete guibg={color8} guifg={color1} ctermbg=8 ctermfg=1
highlight! GitGutterChangeDelete gui=bold guibg={color8} guifg={color1} cterm=bold ctermbg=8 ctermfg=1

" Neomake
" -------
highlight! NeomakeError gui=none guibg={color1} guifg={background} cterm=none ctermbg=1 ctermfg=0
highlight! NeomakeInfo gui=none guibg={color6} guifg={background} cterm=none ctermbg=6 ctermfg=0
highlight! NeomakeWarning gui=none guibg={color3} guifg={background} cterm=none ctermbg=3 ctermfg=0
highlight! NeomakeMessage gui=none guibg={color11} guifg={background} cterm=none ctermbg=11 ctermfg=0

highlight! NeomakeVirtualtextInfoDefault guifg={color14} ctermfg=14
highlight! NeomakeVirtualtextMessageDefault guifg={color11} ctermfg=11
highlight! NeomakeVirtualtextWarningDefault guifg={color3} ctermfg=3
highlight! NeomakeVirtualtextErrorDefault guifg={color1} ctermfg=1

highlight! NeomakeStatusGood gui=none guibg={color2} guifg={background} cterm=none ctermbg=2 ctermfg=0
highlight! NeomakeStatusGoodNC gui=none guibg={color8} guifg={color2} cterm=none ctermbg=8 ctermfg=2

highlight! NeomakeStatColorDefault gui=none guibg={color4} guifg={background} cterm=none ctermbg=4 ctermfg=0
highlight! NeomakeStatColorTypeE gui=none guibg={color1} guifg={background} cterm=none ctermbg=1 ctermfg=0
highlight! NeomakeStatColorTypeW gui=none guibg={color3} guifg={background} cterm=none ctermbg=3 ctermfg=0

" Markdown
" --------
highlight! MarkdownRule gui=bold guibg={color8} guifg={color10} cterm=bold ctermbg=8 ctermfg=10

highlight! MarkdownHeading gui=bold guifg={foreground} cterm=bold ctermfg=15
highlight! default link MarkdownH1 MarkdownHeading
highlight! default link MarkdownH2 MarkdownHeading
highlight! default link MarkdownH3 MarkdownHeading
highlight! default link MarkdownH4 MarkdownHeading
highlight! default link MarkdownH5 MarkdownHeading
highlight! default link MarkdownH6 MarkdownHeading
highlight! default link MarkdownHeadingDelimiter MarkdownHeading
highlight! default link MarkdownHeadingRule MarkdownHeading

highlight! MarkdownBold gui=bold guifg={color9} cterm=bold ctermfg=9
highlight! default link MarkdownBoldDelimiter MarkdownBold

highlight! MarkdownItalic gui=italic guifg={color3} cterm=none ctermfg=3
highlight! default link MarkdownItalicDelimiter MarkdownItalic

highlight! MarkdownUrl gui=underline guifg={color4} cterm=underline ctermfg=4
highlight! MarkdownLinkText gui=none guifg={color12} cterm=none ctermfg=12
highlight! MarkdownLinkDelimiter gui=bold guifg={foreground} cterm=bold ctermfg=15
highlight! default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

highlight! MarkdownCode guifg={color5} ctermfg=5
highlight! default link MarkdownCodeDelimiter MarkdownCode

highlight! MarkdownCodeBlock guifg={foreground} ctermfg=15

highlight! MarkdownListMarker gui=none guifg={color2} cterm=none ctermfg=2
highlight! default link MarkdownOrderedListMarker MarkdownListMarker

" Linting
" -------
highlight! YcmErrorSection gui=undercurl guisp={color1} cterm=underline
highlight! YcmWarningSection gui=undercurl guisp={color3} cterm=underline
highlight! SyntasticError gui=undercurl guisp={color1} cterm=underline
highlight! SyntasticWarning gui=undercurl guisp={color3} cterm=underline
highlight! SyntasticErrorSing guifg={background} guibg={color1} ctermfg=0 ctermbg=1
highlight! SyntasticWarningSign guifg={background} guibg={color3} ctermfg=0 ctermbg=3

" Custom Additions
" ------
highlight! Visual guibg={color12} ctermbg=12
highlight! Search guibg={color3} guifg={background} ctermbg=3 ctermbg=none gui=NONE cterm=NONE
highlight! gitcommitSummary guifg={color14} ctermfg=7
highlight! gitcommitOverflow cterm=bold gui=bold ctermbg=13 guibg={color13} guifg={background} ctermfg=0
highlight! IncSearch ctermbg=9 guibg={color9} ctermfg=0 guifg={background} gui=NONE cterm=NONE
highlight! HighlightedyankRegion ctermbg=9 guibg={color9} ctermfg=0 guifg={background}
highlight! CocErrorSign guibg={color8} guifg={color1} ctermfg=1 ctermbg=8
highlight! CocWarningSign guibg={color8} guifg={color3} ctermfg=3 ctermbg=8
highlight! CocInfoSign guibg={color8} guifg={color1} ctermfg=3 ctermbg=8
highlight! VisualMode guifg={background} ctermfg=0 guibg={color9} ctermbg=9 cterm=bold gui=bold
highlight! InsertMode guifg={background} ctermfg=0 guibg={color2} ctermbg=2 cterm=bold gui=bold
highlight! ReplaceMode guifg={background} ctermfg=0 guibg={color13} ctermbg=13 cterm=bold gui=bold
highlight! CommandMode guifg={background} ctermfg=0 guibg={color6} ctermbg=6 cterm=bold gui=bold
" #C594C5 182
highlight! TerminalMode guifg={background} ctermfg=0 guibg=#CC9CCF ctermbg=182 cterm=bold gui=bold
highlight! NormalMode guifg={background} ctermfg=0 guibg={color4} ctermbg=4 cterm=bold gui=bold
highlight! VisualModeInv guifg={color9} ctermfg=9 guibg={background} ctermbg=none cterm=NONE gui=NONE
highlight! InsertModeInv guifg={color2} ctermfg=2 guibg={background} ctermbg=none cterm=NONE gui=NONE
highlight! ReplaceModeInv guifg={color13} ctermfg=13 guibg={background} ctermbg=none cterm=NONE gui=NONE
highlight! CommandModeInv guifg={color6} ctermfg=6 guibg={background} ctermbg=none cterm=NONE gui=NONE
highlight! TerminalModeInv guifg={color12} ctermfg=12 guibg={background} ctermbg=none cterm=NONE gui=NONE
highlight! NormalModeInv guifg={color4} ctermfg=4 guibg={background} ctermbg=none cterm=NONE gui=NONE
highlight! StatusLine guifg={color8} ctermfg=8 guibg=NONE ctermbg=NONE cterm=NONE gui=NONE
highlight! StatusLineNC guifg={color11} ctermfg=11 guibg={color8} ctermbg=8 cterm=NONE gui=NONE
highlight! StatusLineTerm guifg={color10} ctermfg=10 guibg={color2} ctermbg=2 cterm=NONE gui=NONE
highlight! StatusLineTermNC guifg={color11} ctermfg=11 guibg={color8} ctermbg=8 cterm=NONE gui=NONE
highlight! User1 ctermfg=7 guifg={color7} ctermbg=8 guibg={color8} cterm=NONE gui=NONE
highlight! link User2 Normal
highlight! StatusWarning guifg={color8} ctermfg=8 guibg={color3} ctermbg=3 cterm=NONE gui=NONE
highlight! StatusError guifg={color8} ctermfg=8 guibg={color1} ctermbg=1 cterm=NONE gui=NONE
highlight! StatusOk guifg={foreground} ctermfg=15 guibg={color8} ctermbg=8 cterm=NONE gui=NONE
highlight! StatusWarningInv guibg={background} ctermfg=none guifg={color3} ctermfg=3 cterm=NONE gui=NONE
highlight! StatusErrorInv guibg={background} ctermfg=none guifg={color1} ctermfg=1 cterm=NONE gui=NONE
highlight! StatusOkInv guibg={background} ctermfg=none guifg={color8} ctermfg=8 cterm=NONE gui=NONE
highlight! StatusInfoInv ctermfg=8 guifg={color8} ctermbg=none guibg={background} cterm=NONE gui=NONE
highlight! StatusLineInfo guifg={foreground} ctermfg=15 guibg=#4a4a4b ctermbg=238 cterm=NONE gui=NONE
highlight! StatusLineInfoInv guifg=#4a4a4b ctermfg=238 ctermbg=1 guibg={background} cterm=NONE gui=NONE
highlight! SpellBad gui=underline guifg={foreground} guibg={color8} cterm=underline ctermfg=15 ctermbg=8 guisp={color13}
highlight! ALEError gui=underline guifg={foreground} guibg={color8} cterm=underline ctermfg=15 ctermbg=8 guisp={color1}
highlight! ALEWarning gui=underline guifg={foreground} guibg={color8} cterm=underline ctermfg=15 ctermbg=8 guisp={color3}
highlight! Pmenu gui=none guibg={color8} guifg={foreground} cterm=none ctermbg=8 ctermfg=15
highlight! MarkdownItalic guifg={color13}
highlight! link htmlItalic MarkdownItalic
highlight! link htmlBold MarkdownBold
highlight! mkdBlockquote gui=italic cterm=italic guifg={color7}
" Semishi Formatting: {{{{{{
  highlight! semshiLocal           ctermfg=209 guifg=#ff875f
  highlight! semshiGlobal          ctermfg=214 guifg={color1}
  highlight! semshiImported        ctermfg=214 guifg=#ffaf00 cterm=bold gui=bold
  highlight! semshiParameter       ctermfg=75  guifg={color4}
  highlight! semshiParameterUnused ctermfg=117 guifg={color12} cterm=underline gui=underline
  highlight! semshiFree            ctermfg=218 guifg={color5}
  highlight! semshiBuiltin         ctermfg=207 guifg={color13}
  highlight! semshiAttribute       ctermfg=49  guifg={color6}
  highlight! semshiSelf            ctermfg=249 guifg={color7}
  highlight! semshiUnresolved      ctermfg=226 guifg={color3} cterm=underline gui=underline
  highlight! semshiSelected        ctermfg=231 guifg={background} ctermbg=161 guibg=#ff875f

  highlight! semshiErrorSign       ctermfg=231 guifg={foreground} ctermbg=160 guibg=#d70000
  highlight! semshiErrorChar       ctermfg=231 guifg={foreground} ctermbg=160 guibg=#d70000
" }}}}}}
