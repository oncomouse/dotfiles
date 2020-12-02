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
        hi Normal guibg=#{background.strip} guifg=#{foreground.strip} ctermbg=0 ctermfg=15
else
        " NOTE the ctermbg=none is for terminals with transparency
        hi Normal guibg=#{background.strip} guifg=#{foreground.strip} ctermbg=none ctermfg=15
endif

hi Visual guibg=#{foreground.strip} guifg=#{background.strip} ctermbg=15 ctermfg=0
hi Search gui=underline,bold guibg=#{color8.strip} guifg=#{foreground.strip} cterm=underline,bold ctermbg=8 ctermfg=3
hi IncSearch gui=underline,bold guibg=#{color7.strip} guifg=#{background.strip} term=none cterm=underline,bold ctermbg=7 ctermfg=0

hi StatusLine gui=none guibg=#{foreground.strip} guifg=#{background.strip} cterm=none ctermbg=15 ctermfg=0
hi StatusLineNC gui=none guibg=#{color8.strip} guifg=#{color7.strip} cterm=none ctermbg=8 ctermfg=7
hi StatusLineTerm gui=none guibg=#{color2.strip} guifg=#{background.strip} cterm=none ctermbg=2 ctermfg=0
hi StatusLineTermNC gui=none guibg=#{color8.strip} guifg=#{color2.strip} cterm=none ctermbg=8 ctermfg=2

hi VertSplit gui=none cterm=none
hi TabLine gui=none guibg=#{color8.strip} guifg=#{color7.strip} cterm=none ctermbg=8 ctermfg=7
hi TabLineSel gui=none guibg=#{color6.strip} guifg=#{background.strip} cterm=none ctermbg=6 ctermfg=0
hi TabLineFill gui=none cterm=none

hi Comment gui=italic guifg=#{color7.strip} cterm=none ctermfg=7
hi Todo gui=bold guibg=#{color8.strip} guifg=#{color11.strip} cterm=bold ctermbg=8 ctermfg=11

hi Warning gui=none guibg=#{color3.strip} guifg=#{background.strip} cterm=none ctermbg=3 ctermfg=0
hi WarningMsg gui=none guibg=#{color3.strip} guifg=#{background.strip} cterm=none ctermbg=3 ctermfg=0
hi Error gui=none guibg=#{color1.strip} guifg=#{background.strip} cterm=none ctermbg=1 ctermfg=0
hi ErrorMsg gui=none guibg=#{color1.strip} guifg=#{background.strip} cterm=none ctermbg=1 ctermfg=0

hi MatchParen gui=underline,bold guibg=#{color8.strip} guifg=#{color7.strip} cterm=underline,bold ctermbg=8 ctermfg=7

hi ToolbarLine guibg=#{color7.strip} guifg=#{background.strip} term=none ctermbg=7 ctermfg=0
hi ToolbarButton gui=bold guibg=#{color7.strip} guifg=#{background.strip} term=none cterm=bold ctermbg=7 ctermfg=0

hi WildMenu guibg=#{background.strip} guifg=#{foreground.strip} term=standout ctermbg=0 ctermfg=15

hi Terminal guibg=#{background.strip} guifg=#{foreground.strip} term=none ctermbg=none ctermfg=15

" Constructs
" ----------
hi Constant guifg=#{color4.strip} ctermfg=4
hi Number guifg=#{color4.strip} ctermfg=4
hi Float guifg=#{color4.strip} ctermfg=4
hi String guifg=#{color12.strip} ctermfg=12

hi Function guifg=#{color5.strip} ctermfg=5
hi Identifier guifg=#{color13.strip} term=none ctermfg=13
hi Label guifg=#{color5.strip} ctermfg=5
hi Tag guifg=#{color5.strip} ctermfg=5
hi Keyword gui=bold guifg=#{color13.strip} gui=bold ctermfg=13

hi Character gui=bold guifg=#{color14.strip} cterm=bold ctermfg=14

hi Type gui=none,bold guifg=#{color6.strip} term=none cterm=none,bold ctermfg=6
hi Boolean guifg=#{color6.strip} ctermfg=6
hi StorageClass guifg=#{color6.strip} ctermfg=6
hi Structure guifg=#{color6.strip} ctermfg=6
hi Typedef gui=bold guifg=#{color14.strip} cterm=bold ctermfg=14

hi Conditional gui=bold guifg=#{color2.strip} cterm=bold ctermfg=2
hi Statement gui=none guifg=#{color10.strip} cterm=none ctermfg=10
hi Repeat gui=bold guifg=#{color10.strip} cterm=bold ctermfg=10
hi Operator gui=bold guifg=#{foreground.strip} cterm=bold ctermfg=15
hi Exception gui=bold guifg=#{color1.strip} cterm=bold ctermfg=1

hi Preproc gui=none guifg=#{color9.strip} term=none cterm=none ctermfg=9
hi PreCondit gui=bold guifg=#{color9.strip} cterm=bold ctermfg=9
hi Macro gui=bold guifg=#{color9.strip} cterm=bold ctermfg=9
hi Include guifg=#{color9.strip} ctermfg=9
hi Define guifg=#{color9.strip} ctermfg=9

hi Title gui=bold guibg=#{background.strip} guifg=#{color6.strip} cterm=bold ctermbg=none ctermfg=6

hi Delimeter gui=bold guifg=#{color5.strip} cterm=bold ctermfg=5
hi Delimiter gui=bold guifg=#{color5.strip} cterm=bold ctermfg=5
hi SpecialComment gui=bold guifg=#{color5.strip} cterm=bold ctermfg=5

hi Debug guifg=#{color13.strip} ctermfg=13

" Other
" -----
hi LineNr guibg=#{color8.strip} guifg=#{color7.strip} term=none ctermbg=8 ctermfg=7
hi Cursor guibg=#{foreground.strip} guifg=#{background.strip} ctermbg=15 ctermfg=0
hi CursorLine gui=none guibg=#{color8.strip} term=none cterm=none ctermbg=8
hi CursorColumn gui=none guibg=#{color8.strip} term=none cterm=none ctermbg=8
hi CursorLineNr gui=bold guibg=#{color7.strip} guifg=#{background.strip} cterm=bold ctermbg=7 ctermfg=0
hi ColorColumn guibg=#{color8.strip} guifg=#{foreground.strip} term=none ctermbg=8 ctermfg=15
hi SignColumn guibg=#{color8.strip} guifg=#{color7.strip} term=none ctermbg=8 ctermfg=7

hi Folded guibg=#{color8.strip} guifg=#{color7.strip} ctermbg=8 ctermfg=7
hi FoldColumn guibg=#{color8.strip} guifg=#{color7.strip} ctermbg=8 ctermfg=7

hi Special gui=bold guifg=#{color11.strip} term=none cterm=bold ctermfg=11
hi SpecialKey gui=none guibg=#{color8.strip} guifg=#{color7.strip} cterm=none ctermbg=8 ctermfg=7
hi SpecialChar gui=bold guifg=#{color11.strip} cterm=bold ctermfg=11
hi NonText gui=none guibg=#{color8.strip} guifg=#{color7.strip} cterm=none ctermbg=8 ctermfg=7
hi EndOfBuffer gui=bold guifg=#{color7.strip} cterm=bold ctermfg=7

hi Directory gui=none guifg=#{color2.strip} term=none cterm=none ctermfg=2
hi Question gui=bold guifg=#{color11.strip} cterm=bold ctermfg=11
hi MoreMsg guifg=#{color10.strip} ctermfg=10
hi ModeMsg gui=bold guifg=#{color2.strip} cterm=bold ctermfg=2

hi VimOption guifg=#{color5.strip} ctermfg=5
hi VimGroup guifg=#{color5.strip} ctermfg=5

hi Underlined gui=underline guifg=#{foreground.strip} cterm=underline ctermfg=15
hi Ignore guibg=#{color8.strip} guifg=#{color7.strip} ctermbg=8 ctermfg=7
hi Conceal guibg=#{color7.strip} guifg=#{color8.strip} ctermbg=7 ctermfg=8

hi SpellBad guibg=#{color1.strip} guifg=#{background.strip} ctermbg=1 ctermfg=0
hi SpellCap guibg=#{color3.strip} guifg=#{background.strip} ctermbg=3 ctermfg=0
hi SpellRare guibg=#{color13.strip} guifg=#{background.strip} ctermbg=13 ctermfg=0
hi SpellLocal guibg=#{color14.strip} guifg=#{background.strip} ctermbg=14 ctermfg=0

hi Pmenu gui=italic guibg=#{color8.strip} guifg=#{foreground.strip} cterm=none ctermbg=8 ctermfg=15
hi PmenuSel gui=none,bold guibg=#{color7.strip} guifg=#{background.strip} cterm=none,bold ctermbg=7 ctermfg=0
hi PmenuSbar guibg=#{color8.strip} ctermbg=8
hi PmenuThumb guibg=#{color7.strip} ctermbg=7

" Terminal
" --------

if exists('*term_setansicolors')
        let g:terminal_ansi_colors = [
                                \ '#{background.strip}',
                                \ '#{color1.strip}',
                                \ '#{color2.strip}',
                                \ '#{color3.strip}',
                                \ '#{color4.strip}',
                                \ '#{color5.strip}',
                                \ '#{color6.strip}',
                                \ '#{color7.strip}',
                                \ '#{color8.strip}',
                                \ '#{color9.strip}',
                                \ '#{color10.strip}',
                                \ '#{color11.strip}',
                                \ '#{color12.strip}',
                                \ '#{color13.strip}',
                                \ '#{color14.strip}',
                                \ '#{foreground.strip}'
                                \ ]
endif
if has('nvim')
        let g:terminal_color_0 = '#{background.strip}'
        let g:terminal_color_1 = '#{color1.strip}'
        let g:terminal_color_2 = '#{color2.strip}'
        let g:terminal_color_3 = '#{color3.strip}'
        let g:terminal_color_4 = '#{color4.strip}'
        let g:terminal_color_5 = '#{color5.strip}'
        let g:terminal_color_6 = '#{color6.strip}'
        let g:terminal_color_7 = '#{color7.strip}'
        let g:terminal_color_8 = '#{color8.strip}'
        let g:terminal_color_9 = '#{color9.strip}'
        let g:terminal_color_10 = '#{color10.strip}'
        let g:terminal_color_11 = '#{color11.strip}'
        let g:terminal_color_12 = '#{color12.strip}'
        let g:terminal_color_13 = '#{color13.strip}'
        let g:terminal_color_14 = '#{color14.strip}'
        let g:terminal_color_15 = '#{foreground.strip}'
endif

" Diffs
" -----
hi DiffAdd gui=bold guibg=#{color2.strip} guifg=#{background.strip} cterm=bold ctermbg=10 ctermfg=0
hi DiffDelete gui=none guibg=#{color1.strip} guifg=#{background.strip} cterm=none ctermbg=9 ctermfg=0
hi DiffChange gui=bold guibg=#{color8.strip} guifg=#{color7.strip} cterm=bold ctermbg=8 ctermfg=7
hi DiffText gui=bold guibg=#{color8.strip} guifg=#{color9.strip} cterm=bold ctermbg=8 ctermfg=1

hi diffAdded guifg=#{color2.strip} ctermfg=2
hi diffRemoved guifg=#{color1.strip} ctermfg=1
hi diffNewFile gui=none guifg=#{color4.strip} ctermfg=4
hi diffFile gui=none guifg=#{color3.strip} cterm=none ctermfg=3

hi GitGutterAdd guibg=#{color8.strip} guifg=#{color2.strip} ctermbg=8 ctermfg=2
hi GitGutterChange gui=bold guibg=#{color8.strip} guifg=#{color7.strip} cterm=bold ctermbg=8 ctermfg=7
hi GitGutterDelete guibg=#{color8.strip} guifg=#{color1.strip} ctermbg=8 ctermfg=1
hi GitGutterChangeDelete gui=bold guibg=#{color8.strip} guifg=#{color1.strip} cterm=bold ctermbg=8 ctermfg=1

" Neomake
" -------
hi NeomakeError gui=none guibg=#{color1.strip} guifg=#{background.strip} cterm=none ctermbg=1 ctermfg=0
hi NeomakeInfo gui=none guibg=#{color6.strip} guifg=#{background.strip} cterm=none ctermbg=6 ctermfg=0
hi NeomakeWarning gui=none guibg=#{color3.strip} guifg=#{background.strip} cterm=none ctermbg=3 ctermfg=0
hi NeomakeMessage gui=none guibg=#{color11.strip} guifg=#{background.strip} cterm=none ctermbg=11 ctermfg=0

hi NeomakeVirtualtextInfoDefault guifg=#{color14.strip} ctermfg=14
hi NeomakeVirtualtextMessageDefault guifg=#{color11.strip} ctermfg=11
hi NeomakeVirtualtextWarningDefault guifg=#{color3.strip} ctermfg=3
hi NeomakeVirtualtextErrorDefault guifg=#{color1.strip} ctermfg=1

hi NeomakeStatusGood gui=none guibg=#{color2.strip} guifg=#{background.strip} cterm=none ctermbg=2 ctermfg=0
hi NeomakeStatusGoodNC gui=none guibg=#{color8.strip} guifg=#{color2.strip} cterm=none ctermbg=8 ctermfg=2

hi NeomakeStatColorDefault gui=none guibg=#{color4.strip} guifg=#{background.strip} cterm=none ctermbg=4 ctermfg=0
hi NeomakeStatColorTypeE gui=none guibg=#{color1.strip} guifg=#{background.strip} cterm=none ctermbg=1 ctermfg=0
hi NeomakeStatColorTypeW gui=none guibg=#{color3.strip} guifg=#{background.strip} cterm=none ctermbg=3 ctermfg=0

" Markdown
" --------
hi MarkdownRule gui=bold guibg=#{color8.strip} guifg=#{color10.strip} cterm=bold ctermbg=8 ctermfg=10

hi MarkdownHeading gui=bold guifg=#{foreground.strip} cterm=bold ctermfg=15
hi default link MarkdownH1 MarkdownHeading
hi default link MarkdownH2 MarkdownHeading
hi default link MarkdownH3 MarkdownHeading
hi default link MarkdownH4 MarkdownHeading
hi default link MarkdownH5 MarkdownHeading
hi default link MarkdownH6 MarkdownHeading
hi default link MarkdownHeadingDelimiter MarkdownHeading
hi default link MarkdownHeadingRule MarkdownHeading

hi MarkdownBold gui=bold guifg=#{color9.strip} cterm=bold ctermfg=9
hi default link MarkdownBoldDelimiter MarkdownBold

hi MarkdownItalic gui=italic guifg=#{color3.strip} cterm=none ctermfg=3
hi default link MarkdownItalicDelimiter MarkdownItalic

hi MarkdownUrl gui=underline guifg=#{color4.strip} cterm=underline ctermfg=4
hi MarkdownLinkText gui=none guifg=#{color12.strip} cterm=none ctermfg=12
hi MarkdownLinkDelimiter gui=bold guifg=#{foreground.strip} cterm=bold ctermfg=15
hi default link MarkdownLinkTextDelimiter MarkdownLinkDelimiter

hi MarkdownCode guifg=#{color5.strip} ctermfg=5
hi default link MarkdownCodeDelimiter MarkdownCode

hi MarkdownCodeBlock guifg=#{foreground.strip} ctermfg=15

hi MarkdownListMarker gui=none guifg=#{color2.strip} cterm=none ctermfg=2
hi default link MarkdownOrderedListMarker MarkdownListMarker

" Linting
" -------
hi YcmErrorSection gui=undercurl guisp=#{color1.strip} cterm=underline
hi YcmWarningSection gui=undercurl guisp=#{color3.strip} cterm=underline
hi SyntasticError gui=undercurl guisp=#{color1.strip} cterm=underline
hi SyntasticWarning gui=undercurl guisp=#{color3.strip} cterm=underline
hi SyntasticErrorSing guifg=#{background.strip} guibg=#{color1.strip} ctermfg=0 ctermbg=1
hi SyntasticWarningSign guifg=#{background.strip} guibg=#{color3.strip} ctermfg=0 ctermbg=3

" Custom Additions
" ------
hi Visual guibg=#{color12.strip} ctermbg=12
hi Search guibg=#{color3.strip} guifg=#{background.strip} ctermbg=3 ctermbg=none gui=NONE cterm=NONE
hi gitcommitSummary guifg=#{color14.strip} ctermfg=7
hi gitcommitOverflow cterm=bold gui=bold ctermbg=13 guibg=#{color13.strip} guifg=#{background.strip} ctermfg=0
hi IncSearch ctermbg=9 guibg=#{color9.strip} ctermfg=0 guifg=#{background.strip} gui=NONE cterm=NONE
hi HighlightedyankRegion ctermbg=9 guibg=#{color9.strip} ctermfg=0 guifg=#{background.strip}
hi CocErrorSign guibg=#{color8.strip} guifg=#{color1.strip} ctermfg=1 ctermbg=8
hi CocWarningSign guibg=#{color8.strip} guifg=#{color3.strip} ctermfg=3 ctermbg=8
hi CocInfoSign guibg=#{color8.strip} guifg=#{color1.strip} ctermfg=3 ctermbg=8
hi VisualMode guifg=#{color8.strip} ctermfg=8 guibg=#{color9.strip} ctermbg=9 cterm=bold gui=bold
hi InsertMode guifg=#{color8.strip} ctermfg=8 guibg=#{color2.strip} ctermbg=2 cterm=bold gui=bold
hi ReplaceMode guifg=#{color8.strip} ctermfg=8 guibg=#{color13.strip} ctermbg=13 cterm=bold gui=bold
hi CommandMode guifg=#{color8.strip} ctermfg=8 guibg=#{color6.strip} ctermbg=6 cterm=bold gui=bold
" #C594C5 182
hi TerminalMode guifg=#{color8.strip} ctermfg=8 guibg=#CC9CCF ctermbg=182 cterm=bold gui=bold
hi NormalMode guifg=#{color8.strip} ctermfg=8 guibg=#{color4.strip} ctermbg=4 cterm=bold gui=bold
hi VisualModeInv guifg=#{color9.strip} ctermfg=9 guibg=#{background.strip} ctermbg=none cterm=NONE gui=NONE
hi InsertModeInv guifg=#{color2.strip} ctermfg=2 guibg=#{background.strip} ctermbg=none cterm=NONE gui=NONE
hi ReplaceModeInv guifg=#{color13.strip} ctermfg=13 guibg=#{background.strip} ctermbg=none cterm=NONE gui=NONE
hi CommandModeInv guifg=#{color6.strip} ctermfg=6 guibg=#{background.strip} ctermbg=none cterm=NONE gui=NONE
hi TerminalModeInv guifg=#{color12.strip} ctermfg=12 guibg=#{background.strip} ctermbg=none cterm=NONE gui=NONE
hi NormalModeInv guifg=#{color4.strip} ctermfg=4 guibg=#{background.strip} ctermbg=none cterm=NONE gui=NONE
hi StatusLine guifg=#{color8.strip} ctermfg=8 guibg=NONE ctermbg=NONE cterm=NONE gui=NONE
hi StatusLineNC guifg=#{color11.strip} ctermfg=11 guibg=#{color8.strip} ctermbg=8 cterm=NONE gui=NONE
hi StatusLineTerm guifg=#{color10.strip} ctermfg=10 guibg=#{color2.strip} ctermbg=2 cterm=NONE gui=NONE
hi StatusLineTermNC guifg=#{color11.strip} ctermfg=11 guibg=#{color8.strip} ctermbg=8 cterm=NONE gui=NONE
hi User1 ctermfg=7 guifg=#{color7.strip} ctermbg=8 guibg=#{color8.strip} cterm=NONE gui=NONE
hi link User2 Normal
hi StatusWarning guifg=#{color8.strip} ctermfg=8 guibg=#{color3.strip} ctermbg=3 cterm=NONE gui=NONE
hi StatusError guifg=#{color8.strip} ctermfg=8 guibg=#{color1.strip} ctermbg=1 cterm=NONE gui=NONE
hi StatusOk guifg=#{foreground.strip} ctermfg=15 guibg=#{color8.strip} ctermbg=8 cterm=NONE gui=NONE
hi StatusWarningInv guibg=#{background.strip} ctermfg=none guifg=#{color3.strip} ctermfg=3 cterm=NONE gui=NONE
hi StatusErrorInv guibg=#{background.strip} ctermfg=none guifg=#{color1.strip} ctermfg=1 cterm=NONE gui=NONE
hi StatusOkInv guibg=#{background.strip} ctermfg=none guifg=#{color8.strip} ctermfg=8 cterm=NONE gui=NONE
hi StatusInfoInv ctermfg=8 guifg=#{color8.strip} ctermbg=none guibg=#{background.strip} cterm=NONE gui=NONE
hi StatusLineInfo guifg=#{foreground.strip} ctermfg=15 guibg=#4a4a4b ctermbg=238 cterm=NONE gui=NONE
hi StatusLineInfoInv guifg=#4a4a4b ctermfg=238 ctermbg=1 guibg=#{background.strip} cterm=NONE gui=NONE
hi SpellBad gui=underline guifg=#{foreground.strip} guibg=#{color8.strip} cterm=underline ctermfg=15 ctermbg=8 guisp=#{color13.strip}
hi ALEError gui=underline guifg=#{foreground.strip} guibg=#{color8.strip} cterm=underline ctermfg=15 ctermbg=8 guisp=#{color1.strip}
hi ALEWarning gui=underline guifg=#{foreground.strip} guibg=#{color8.strip} cterm=underline ctermfg=15 ctermbg=8 guisp=#{color3.strip}
hi Pmenu gui=none guibg=#{color8.strip} guifg=#{foreground.strip} cterm=none ctermbg=8 ctermfg=15
hi MarkdownItalic guifg=#{color13.strip}
hi link htmlItalic MarkdownItalic
hi link htmlBold MarkdownBold
hi mkdBlockquote gui=italic cterm=italic guifg=#{color7.strip}
" Semishi Formatting: {{{{{{
  hi semshiLocal           ctermfg=209 guifg=#ff875f
  hi semshiGlobal          ctermfg=214 guifg=#{color1.strip}
  hi semshiImported        ctermfg=214 guifg=#ffaf00 cterm=bold gui=bold
  hi semshiParameter       ctermfg=75  guifg=#{color4.strip}
  hi semshiParameterUnused ctermfg=117 guifg=#{color12.strip} cterm=underline gui=underline
  hi semshiFree            ctermfg=218 guifg=#{color5.strip}
  hi semshiBuiltin         ctermfg=207 guifg=#{color13.strip}
  hi semshiAttribute       ctermfg=49  guifg=#{color6.strip}
  hi semshiSelf            ctermfg=249 guifg=#{color7.strip}
  hi semshiUnresolved      ctermfg=226 guifg=#{color3.strip} cterm=underline gui=underline
  hi semshiSelected        ctermfg=231 guifg=#{background.strip} ctermbg=161 guibg=#ff875f

  hi semshiErrorSign       ctermfg=231 guifg=#{foreground.strip} ctermbg=160 guibg=#d70000
  hi semshiErrorChar       ctermfg=231 guifg=#{foreground.strip} ctermbg=160 guibg=#d70000
" }}}}}}
