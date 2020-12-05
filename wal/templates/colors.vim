" Name: Tempus Classic
" Description: Dark theme with warm hues (WCAG AA compliant)
" Author: Protesilaos Stavrou (https://protesilaos.com)
" Meta: Created with the Tempus Themes Generator
" URL: https://gitlab.com/protesilaos/tempus-themes-generator

set background=dark
hi clear
if exists('syntax_on')
        syntax reset
endif
let g:colors_name = 'tempus_wal'

" General
" -------
if exists('g:tempus_enforce_background_color')
        hi Normal guibg={background} guifg={foreground} ctermbg=0 ctermfg=15
else
        " NOTE the ctermbg=none is for terminals with transparency
        hi Normal guibg={background} guifg={foreground} ctermbg=none ctermfg=15
endif

hi Visual guibg={foreground} guifg={background} ctermbg=15 ctermfg=0
hi Search gui=underline,bold guibg={color8} guifg={foreground} cterm=underline,bold ctermbg=8 ctermfg=3
hi IncSearch gui=underline,bold guibg={color7} guifg={background} term=none cterm=underline,bold ctermbg=7 ctermfg=0

hi StatusLine gui=none guibg={foreground} guifg={background} cterm=none ctermbg=15 ctermfg=0
hi StatusLineNC gui=none guibg={color8} guifg={color7} cterm=none ctermbg=8 ctermfg=7
hi StatusLineTerm gui=none guibg={color2} guifg={background} cterm=none ctermbg=2 ctermfg=0
hi StatusLineTermNC gui=none guibg={color8} guifg={color2} cterm=none ctermbg=8 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg={color8} guifg={color7} cterm=none ctermbg=8 ctermfg=7
hi TabLineSel gui=none guibg={color6} guifg={background} cterm=none ctermbg=6 ctermfg=0
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg={color7} cterm=none ctermfg=7
hi Todo gui=bold guibg={color8} guifg={color11} cterm=bold ctermbg=8 ctermfg=11

hi Warning gui=none guibg={color3} guifg={background} cterm=none ctermbg=3 ctermfg=0
hi WarningMsg gui=none guibg={color3} guifg={background} cterm=none ctermbg=3 ctermfg=0
hi Error gui=none guibg={color1} guifg={background} cterm=none ctermbg=1 ctermfg=0
hi ErrorMsg gui=none guibg={color1} guifg={background} cterm=none ctermbg=1 ctermfg=0

hi MatchParen gui=underline,bold guibg={color8} guifg={color7} cterm=underline,bold ctermbg=8 ctermfg=7

hi ToolbarLine guibg={color7} guifg={background} term=none ctermbg=7 ctermfg=0
hi ToolbarButton gui=bold guibg={color7} guifg={background} term=none cterm=bold ctermbg=7 ctermfg=0

hi WildMenu guibg={background} guifg={foreground} term=standout ctermbg=0 ctermfg=15

hi Terminal guibg={background} guifg={foreground} term=none ctermbg=none ctermfg=15

" Constructs
" ----------
hi Constant guifg={color4} ctermfg=4
hi Number guifg={color4} ctermfg=4
hi Float guifg={color4} ctermfg=4
hi String guifg={color12} ctermfg=12

hi Function guifg={color5} ctermfg=5
hi Identifier guifg={color13} term=none ctermfg=13
hi Label guifg={color5} ctermfg=5
hi Tag guifg={color5} ctermfg=5
hi Keyword gui=bold guifg={color13} gui=bold ctermfg=13

hi Character gui=bold guifg={color14} cterm=bold ctermfg=14

hi Type gui=none,bold guifg={color6} term=none cterm=none,bold ctermfg=6
hi Boolean guifg={color6} ctermfg=6
hi StorageClass guifg={color6} ctermfg=6
hi Structure guifg={color6} ctermfg=6
hi Typedef gui=bold guifg={color14} cterm=bold ctermfg=14

hi Conditional gui=bold guifg={color2} cterm=bold ctermfg=2
hi Statement gui=none guifg={color10} cterm=none ctermfg=10
hi Repeat gui=bold guifg={color10} cterm=bold ctermfg=10
hi Operator gui=bold guifg={foreground} cterm=bold ctermfg=15
hi Exception gui=bold guifg={color1} cterm=bold ctermfg=1

hi Preproc gui=none guifg={color9} term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg={color9} cterm=bold ctermfg=9
hi Macro gui=bold guifg={color9} cterm=bold ctermfg=9
hi Include guifg={color9} ctermfg=9
hi Define guifg={color9} ctermfg=9

hi Title gui=bold guibg={background} guifg={color6} cterm=bold ctermbg=none ctermfg=6

hi Delimeter gui=bold guifg={color5} cterm=bold ctermfg=5
hi Delimiter gui=bold guifg={color5} cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg={color5} cterm=bold ctermfg=5

hi Debug guifg={color13} ctermfg=13

" Other
" -----
hi LineNr guibg={color8} guifg={color7} term=none ctermbg=8 ctermfg=7
hi Cursor guibg={foreground} guifg={background} ctermbg=15 ctermfg=0
hi CursorLine gui=none guibg={color8} term=none cterm=none ctermbg=8
hi CursorColumn gui=none guibg={color8} term=none cterm=none ctermbg=8
hi CursorLineNr gui=bold guibg={color7} guifg={background} cterm=bold ctermbg=7 ctermfg=0
hi ColorColumn guibg={color8} guifg={foreground} term=none ctermbg=8 ctermfg=15
hi SignColumn guibg={color8} guifg={color7} term=none ctermbg=8 ctermfg=7

hi Folded guibg={color8} guifg={color7} ctermbg=8 ctermfg=7
hi FoldColumn guibg={color8} guifg={color7} ctermbg=8 ctermfg=7

hi Special gui=bold guifg={color11} term=none cterm=bold ctermfg=11
hi SpecialKey gui=none guibg={color8} guifg={color7} cterm=none ctermbg=8 ctermfg=7
hi SpecialChar gui=bold guifg={color11} cterm=bold ctermfg=11
hi NonText gui=none guibg={color8} guifg={color7} cterm=none ctermbg=8 ctermfg=7
hi EndOfBuffer gui=bold guifg={color7} cterm=bold ctermfg=7

hi Directory gui=none guifg={color2} term=none cterm=none ctermfg=2
hi Question gui=bold guifg={color11} cterm=bold ctermfg=11
hi MoreMsg guifg={color10} ctermfg=10
hi ModeMsg gui=bold guifg={color2} cterm=bold ctermfg=2

hi VimOption guifg={color5} ctermfg=5
hi VimGroup guifg={color5} ctermfg=5

hi Underlined gui=underline guifg={foreground} cterm=underline ctermfg=15
hi Ignore guibg={color8} guifg={color7} ctermbg=8 ctermfg=7
hi Conceal guibg={color7} guifg={color8} ctermbg=7 ctermfg=8

hi SpellBad guibg={color1} guifg={background} ctermbg=1 ctermfg=0
hi SpellCap guibg={color3} guifg={background} ctermbg=3 ctermfg=0
hi SpellRare guibg={color13} guifg={background} ctermbg=13 ctermfg=0
hi SpellLocal guibg={color14} guifg={background} ctermbg=14 ctermfg=0

hi Pmenu gui=italic guibg={color8} guifg={foreground} cterm=none ctermbg=8 ctermfg=15
hi PmenuSel gui=none,bold guibg={color7} guifg={background} cterm=none,bold ctermbg=7 ctermfg=0
hi PmenuSbar guibg={color8} ctermbg=8
hi PmenuThumb guibg={color7} ctermbg=7

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
hi DiffAdd gui=bold guibg={color2} guifg={background} cterm=bold ctermbg=10 ctermfg=0
hi DiffDelete gui=none guibg={color1} guifg={background} cterm=none ctermbg=9 ctermfg=0
hi DiffChange gui=bold guibg={color8} guifg={color7} cterm=bold ctermbg=8 ctermfg=7
hi DiffText gui=bold guibg={color8} guifg={color9} cterm=bold ctermbg=8 ctermfg=1

hi diffAdded guifg={color2} ctermfg=2
hi diffRemoved guifg={color1} ctermfg=1
hi diffNewFile gui=none guifg={color4} ctermfg=4
hi diffFile gui=none guifg={color3} cterm=none ctermfg=3

hi GitGutterAdd guibg={color8} guifg={color2} ctermbg=8 ctermfg=2
hi GitGutterChange gui=bold guibg={color8} guifg={color7} cterm=bold ctermbg=8 ctermfg=7
hi GitGutterDelete guibg={color8} guifg={color1} ctermbg=8 ctermfg=1
hi GitGutterChangeDelete gui=bold guibg={color8} guifg={color1} cterm=bold ctermbg=8 ctermfg=1

" Neomake
" -------
hi NeomakeError gui=none guibg={color1} guifg={background} cterm=none ctermbg=1 ctermfg=0
hi NeomakeInfo gui=none guibg={color6} guifg={background} cterm=none ctermbg=6 ctermfg=0
hi NeomakeWarning gui=none guibg={color3} guifg={background} cterm=none ctermbg=3 ctermfg=0
hi NeomakeMessage gui=none guibg={color11} guifg={background} cterm=none ctermbg=11 ctermfg=0

hi NeomakeVirtualtextInfoDefault guifg={color14} ctermfg=14
hi NeomakeVirtualtextMessageDefault guifg={color11} ctermfg=11
hi NeomakeVirtualtextWarningDefault guifg={color3} ctermfg=3
hi NeomakeVirtualtextErrorDefault guifg={color1} ctermfg=1

hi NeomakeStatusGood gui=none guibg={color2} guifg={background} cterm=none ctermbg=2 ctermfg=0
hi NeomakeStatusGoodNC gui=none guibg={color8} guifg={color2} cterm=none ctermbg=8 ctermfg=2

hi NeomakeStatColorDefault gui=none guibg={color4} guifg={background} cterm=none ctermbg=4 ctermfg=0
hi NeomakeStatColorTypeE gui=none guibg={color1} guifg={background} cterm=none ctermbg=1 ctermfg=0
hi NeomakeStatColorTypeW gui=none guibg={color3} guifg={background} cterm=none ctermbg=3 ctermfg=0

" Markdown
" --------
hi MarkdownRule gui=bold guibg={color8} guifg={color10} cterm=bold ctermbg=8 ctermfg=10

hi MarkdownHeading gui=bold guifg={foreground} cterm=bold ctermfg=15
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg={color9} cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg={color3} cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg={color4} cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg={color12} cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg={foreground} cterm=bold ctermfg=15
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg={color5} ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg={foreground} ctermfg=15

hi MarkdownListMarker gui=none guifg={color2} cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker

" Linting
" -------
hi YcmErrorSection gui=undercurl guisp={color1} cterm=underline
hi YcmWarningSection gui=undercurl guisp={color3} cterm=underline
hi SyntasticError gui=undercurl guisp={color1} cterm=underline
hi SyntasticWarning gui=undercurl guisp={color3} cterm=underline
hi SyntasticErrorSing guifg={background} guibg={color1} ctermfg=0 ctermbg=1
hi SyntasticWarningSign guifg={background} guibg={color3} ctermfg=0 ctermbg=3

" Custom Additions
" ------
hi Visual guibg={color12} ctermbg=12
hi Search guibg={color3} guifg={background} ctermbg=3 ctermbg=none gui=NONE cterm=NONE
hi gitcommitSummary guifg={color14} ctermfg=7
hi gitcommitOverflow cterm=bold gui=bold ctermbg=13 guibg={color13} guifg={background} ctermfg=0
hi IncSearch ctermbg=9 guibg={color9} ctermfg=0 guifg={background} gui=NONE cterm=NONE
hi HighlightedyankRegion ctermbg=9 guibg={color9} ctermfg=0 guifg={background}
hi CocErrorSign guibg={color8} guifg={color1} ctermfg=1 ctermbg=8
hi CocWarningSign guibg={color8} guifg={color3} ctermfg=3 ctermbg=8
hi CocInfoSign guibg={color8} guifg={color1} ctermfg=3 ctermbg=8
hi VisualMode guifg={background} ctermfg=0 guibg={color9} ctermbg=9 cterm=bold gui=bold
hi InsertMode guifg={background} ctermfg=0 guibg={color2} ctermbg=2 cterm=bold gui=bold
hi ReplaceMode guifg={background} ctermfg=0 guibg={color13} ctermbg=13 cterm=bold gui=bold
hi CommandMode guifg={background} ctermfg=0 guibg={color6} ctermbg=6 cterm=bold gui=bold
" #C594C5 182
hi TerminalMode guifg={background} ctermfg=0 guibg=#CC9CCF ctermbg=182 cterm=bold gui=bold
hi NormalMode guifg={background} ctermfg=0 guibg={color4} ctermbg=4 cterm=bold gui=bold
hi VisualModeInv guifg={color9} ctermfg=9 guibg={background} ctermbg=none cterm=NONE gui=NONE
hi InsertModeInv guifg={color2} ctermfg=2 guibg={background} ctermbg=none cterm=NONE gui=NONE
hi ReplaceModeInv guifg={color13} ctermfg=13 guibg={background} ctermbg=none cterm=NONE gui=NONE
hi CommandModeInv guifg={color6} ctermfg=6 guibg={background} ctermbg=none cterm=NONE gui=NONE
hi TerminalModeInv guifg={color12} ctermfg=12 guibg={background} ctermbg=none cterm=NONE gui=NONE
hi NormalModeInv guifg={color4} ctermfg=4 guibg={background} ctermbg=none cterm=NONE gui=NONE
hi StatusLine guifg={color8} ctermfg=8 guibg=NONE ctermbg=NONE cterm=NONE gui=NONE
hi StatusLineNC guifg={color11} ctermfg=11 guibg={color8} ctermbg=8 cterm=NONE gui=NONE
hi StatusLineTerm guifg={color10} ctermfg=10 guibg={color2} ctermbg=2 cterm=NONE gui=NONE
hi StatusLineTermNC guifg={color11} ctermfg=11 guibg={color8} ctermbg=8 cterm=NONE gui=NONE
hi User1 ctermfg=7 guifg={color7} ctermbg=8 guibg={color8} cterm=NONE gui=NONE
hi link User2 Normal
hi StatusWarning guifg={color8} ctermfg=8 guibg={color3} ctermbg=3 cterm=NONE gui=NONE
hi StatusError guifg={color8} ctermfg=8 guibg={color1} ctermbg=1 cterm=NONE gui=NONE
hi StatusOk guifg={foreground} ctermfg=15 guibg={color8} ctermbg=8 cterm=NONE gui=NONE
hi StatusWarningInv guibg={background} ctermfg=none guifg={color3} ctermfg=3 cterm=NONE gui=NONE
hi StatusErrorInv guibg={background} ctermfg=none guifg={color1} ctermfg=1 cterm=NONE gui=NONE
hi StatusOkInv guibg={background} ctermfg=none guifg={color8} ctermfg=8 cterm=NONE gui=NONE
hi StatusInfoInv ctermfg=8 guifg={color8} ctermbg=none guibg={background} cterm=NONE gui=NONE
hi StatusLineInfo guifg={foreground} ctermfg=15 guibg=#4a4a4b ctermbg=238 cterm=NONE gui=NONE
hi StatusLineInfoInv guifg=#4a4a4b ctermfg=238 ctermbg=1 guibg={background} cterm=NONE gui=NONE
hi SpellBad gui=underline guifg={foreground} guibg={color8} cterm=underline ctermfg=15 ctermbg=8 guisp={color13}
hi ALEError gui=underline guifg={foreground} guibg={color8} cterm=underline ctermfg=15 ctermbg=8 guisp={color1}
hi ALEWarning gui=underline guifg={foreground} guibg={color8} cterm=underline ctermfg=15 ctermbg=8 guisp={color3}
hi Pmenu gui=none guibg={color8} guifg={foreground} cterm=none ctermbg=8 ctermfg=15
hi MarkdownItalic guifg={color13}
hi link htmlItalic MarkdownItalic
hi link htmlBold MarkdownBold
hi mkdBlockquote gui=italic cterm=italic guifg={color7}
" Semishi Formatting: {{{{{{
  hi semshiLocal           ctermfg=209 guifg=#ff875f
  hi semshiGlobal          ctermfg=214 guifg={color1}
  hi semshiImported        ctermfg=214 guifg=#ffaf00 cterm=bold gui=bold
  hi semshiParameter       ctermfg=75  guifg={color4}
  hi semshiParameterUnused ctermfg=117 guifg={color12} cterm=underline gui=underline
  hi semshiFree            ctermfg=218 guifg={color5}
  hi semshiBuiltin         ctermfg=207 guifg={color13}
  hi semshiAttribute       ctermfg=49  guifg={color6}
  hi semshiSelf            ctermfg=249 guifg={color7}
  hi semshiUnresolved      ctermfg=226 guifg={color3} cterm=underline gui=underline
  hi semshiSelected        ctermfg=231 guifg={background} ctermbg=161 guibg=#ff875f

  hi semshiErrorSign       ctermfg=231 guifg={foreground} ctermbg=160 guibg=#d70000
  hi semshiErrorChar       ctermfg=231 guifg={foreground} ctermbg=160 guibg=#d70000
" }}}}}}
