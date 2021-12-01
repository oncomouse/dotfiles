set background=dark
if exists('g:colors_name')
	hi clear
	if exists('syntax_on')
	  syntax reset
	endif
endif
let g:colors_name = 'wal'
hi Bold cterm=bold gui=bold 
hi Debug ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi Directory ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi ErrorMsg ctermfg=5 guifg={color5} ctermbg=235 guibg={base00} cterm=NONE gui=NONE
hi Exception ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi FoldColumn ctermfg=4 guifg={color4} ctermbg=235 guibg={base00} cterm=NONE gui=NONE
hi Folded ctermfg=145 guifg={base04} ctermbg=237 guibg={base01} cterm=italic gui=italic 
hi IncSearch ctermfg=237 guifg={base01} ctermbg=1 guibg={color1} cterm=NONE gui=NONE 
hi Italic cterm=italic gui=italic 

hi Macro ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi MatchParen ctermfg=251 guifg={base05} ctermbg=243 guibg={base03} cterm=NONE gui=NONE
hi ModeMsg ctermfg=2 guifg={color2} cterm=NONE gui=NONE
hi MoreMsg ctermfg=2 guifg={color2} cterm=NONE gui=NONE
hi Question ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi Search ctermfg=243 guifg={base03} ctermbg=3 guibg={color3} cterm=NONE gui=NONE
hi SpecialKey ctermfg=243 guifg={base03} cterm=NONE gui=NONE
hi TooLong ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi Underlined ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi Visual ctermbg=240 guibg={base02} ctermfg=235 guifg={base00} cterm=NONE gui=NONE
hi VisualNOS ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi WarningMsg ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi WildMenu ctermfg=251 guifg={base05} ctermbg=4 guibg={color4} cterm=NONE gui=NONE
hi Title ctermfg=4 guifg={color4} cterm=bold gui=bold 
hi Conceal ctermfg=237 guifg={base01} ctermbg=235 guibg={base00} cterm=NONE gui=NONE
hi Cursor ctermfg=235 guifg={base00} ctermbg=251 guibg={base05} cterm=NONE gui=NONE
hi NonText ctermfg=243 guifg={base03} cterm=NONE gui=NONE
hi Normal ctermfg=251 guifg={base05} ctermbg=235 guibg={base00} cterm=NONE gui=NONE
hi EndOfBuffer ctermfg=251 guifg={base05} ctermbg=235 guibg={base00} cterm=NONE gui=NONE
hi LineNr ctermfg=243 guifg={base03} ctermbg=235 guibg={base00} cterm=NONE gui=NONE
hi SignColumn ctermfg=240 guifg={base02} ctermbg=235 guibg={base00} cterm=NONE gui=NONE
hi VertSplit ctermfg=235 guifg={base00} ctermbg=240 guibg={base02} cterm=NONE gui=NONE
hi ColorColumn ctermfg=240 guifg={base02} cterm=NONE gui=NONE
hi CursorColumn ctermfg=240 guifg={base02} cterm=NONE gui=NONE
hi CursorLine ctermfg=237 guifg={base01} cterm=None gui=None 
hi CursorLineNr ctermfg=145 guifg={base04} ctermbg=235 guibg={base00} cterm=None gui=None
hi PMenu ctermfg=145 guifg={base04} ctermbg=237 guibg={base01} cterm=NONE gui=NONE
hi PMenuSel ctermfg=251 guifg={base05} ctermbg=4 guibg={color4} cterm=NONE gui=NONE
hi PmenuSbar ctermfg=240 guifg={base02} cterm=NONE gui=NONE
hi PmenuThumb ctermfg=251 guifg={base05} cterm=NONE gui=NONE
hi TabLine ctermfg=243 guifg={base03} ctermbg=237 guibg={base01} cterm=NONE gui=NONE
hi TabLineFill ctermfg=243 guifg={base03} ctermbg=237 guibg={base01} cterm=NONE gui=NONE
hi TabLineSel ctermfg=2 guifg={color2} ctermbg=237 guibg={base01} cterm=NONE gui=NONE
hi helpExample ctermfg=3 guifg={color3} cterm=NONE gui=NONE
hi helpCommand ctermfg=3 guifg={color3} cterm=NONE gui=NONE

hi Boolean ctermfg=1 guifg={color1} cterm=NONE gui=NONE
hi Character ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi Comment ctermfg=145 guifg={base04} cterm=italic gui=italic 
hi Conditional ctermfg=176 guifg={base0E} cterm=NONE gui=NONE
hi Constant ctermfg=1 guifg={color1} cterm=NONE gui=NONE
hi Define ctermfg=176 guifg={base0E} cterm=NONE gui=NONE
hi Delimiter ctermfg=9 guifg={base0F} cterm=NONE gui=NONE
hi Float ctermfg=1 guifg={color1} cterm=NONE gui=NONE
hi Function ctermfg=4 guifg={color4} cterm=NONE gui=NONE

hi Identifier ctermfg=6 guifg={color6} cterm=NONE gui=NONE
hi Include ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi Keyword ctermfg=176 guifg={base0E} cterm=NONE gui=NONE

hi Label ctermfg=3 guifg={color3} cterm=NONE gui=NONE
hi Number ctermfg=1 guifg={color1} cterm=NONE gui=NONE
hi Operator ctermfg=251 guifg={base05} cterm=NONE gui=NONE
hi PreProc ctermfg=3 guifg={color3} cterm=NONE gui=NONE
hi Repeat ctermfg=3 guifg={color3} cterm=NONE gui=NONE
hi Special ctermfg=6 guifg={color6} cterm=NONE gui=NONE
hi SpecialChar ctermfg=9 guifg={base0F} cterm=NONE gui=NONE
hi Statement ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi StorageClass ctermfg=3 guifg={color3} cterm=NONE gui=NONE
hi String ctermfg=2 guifg={color2} cterm=NONE gui=NONE
hi Structure ctermfg=176 guifg={base0E} cterm=NONE gui=NONE
hi Tag ctermfg=3 guifg={color3} cterm=NONE gui=NONE
hi Todo ctermfg=3 guifg={color3} ctermbg=237 guibg={base01} cterm=NONE gui=NONE
hi Type ctermfg=3 guifg={color3} cterm=NONE gui=NONE
hi Typedef ctermfg=3 guifg={color3} cterm=NONE gui=NONE

hi DiagnosticError ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi DiagnosticWarn ctermfg=3 guifg={color3} cterm=NONE gui=NONE
hi DiagnosticInfo ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi DiagnosticHint ctermfg=6 guifg={color6} cterm=NONE gui=NONE
hi DiagnosticUnderlineError ctermfg=5 guifg={color5} cterm=underline gui=underline guisp={color5}
hi DiagnosticUnderlineWarn ctermfg=3 guifg={color3} cterm=underline gui=underline guisp={color3}
hi DiagnosticUnderlineInfo ctermfg=4 guifg={color4} cterm=underline gui=underline guisp={color4}
hi DiagnosticUnderlineHint ctermfg=6 guifg={color6} cterm=underline gui=underline guisp={color6}

hi TSAnnotation ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi TSAttribute ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi TSBoolean ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi TSCharacter ctermfg=3 guifg={color3} cterm=NONE gui=NONE
hi TSComment ctermfg=145 guifg={base04} cterm=italic gui=italic 
hi TSConditional ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi TSConstBuiltin ctermfg=1 guifg={color1} cterm=NONE gui=NONE
hi TSConstMacro ctermfg=1 guifg={color1} cterm=NONE gui=NONE
hi TSConstant ctermfg=1 guifg={color1} cterm=NONE gui=NONE
hi TSConstructor ctermfg=252 guifg={base05} cterm=NONE gui=NONE
hi TSEmphasis cterm=bold gui=bold 
hi TSError ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi TSException ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi TSField ctermfg=2 guifg={color2} cterm=NONE gui=NONE
hi TSFloat ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi TSFuncBuiltin ctermfg=1 guifg={color1} cterm=NONE gui=NONE
hi TSFuncMacro ctermfg=1 guifg={color1} cterm=NONE gui=NONE
hi TSFunction ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi TSInclude ctermfg=6 guifg={color6} cterm=NONE gui=NONE
hi TSKeyword ctermfg=98 guifg={base0E} cterm=NONE gui=NONE
hi TSKeywordFunction ctermfg=6 guifg={color6} cterm=NONE gui=NONE
hi TSKeywordOperator ctermfg=98 guifg={base0E} cterm=NONE gui=NONE
hi TSLabel ctermfg=6 guifg={color6} 
hi TSMethod ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi TSNamespace ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi TSNumber ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi TSOperator ctermfg=252 guifg={base05} cterm=NONE gui=NONE
hi TSParameter ctermfg=3 guifg={color3} cterm=NONE gui=NONE
hi TSParameterReference ctermfg=1 guifg={color1} cterm=NONE gui=NONE
hi TSProperty ctermfg=3 guifg={color3} cterm=NONE gui=NONE
hi TSPunctBracket ctermfg=252 guifg={base05} cterm=NONE gui=NONE
hi TSPunctDelimiter ctermfg=252 guifg={base05} cterm=NONE gui=NONE
hi TSPunctSpecial ctermfg=252 guifg={base05} cterm=NONE gui=NONE
hi TSRepeat ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi TSString ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi TSStringEscape ctermfg=2 guifg={color2} cterm=NONE gui=NONE
hi TSStringRegex ctermfg=2 guifg={color2} cterm=NONE gui=NONE
hi TSStructure ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi TSTag ctermfg=3 guifg={color3} cterm=NONE gui=NONE
hi TSTagDelimiter ctermfg=6 guifg={color6} cterm=NONE gui=NONE
hi TSText ctermfg=2 guifg={color2} cterm=NONE gui=NONE
hi TSType ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi TSTypeBuiltin ctermfg=4 guifg={color4} 
hi TSURI cterm=underline gui=underline ctermbg=237 guibg={base01} 
hi TSUnderline cterm=underline gui=underline
hi TSVariable ctermfg=3 guifg={color3} cterm=NONE gui=NONE
hi TSVariableBuiltin ctermfg=5 guifg={color5} cterm=NONE gui=NONE

hi SpellBad cterm=underline gui=underline guifg={color5} guisp={color5}
hi SpellLocal cterm=underline gui=underline guifg={color6} guisp={color6}
hi SpellCap cterm=underline gui=underline guifg={color3} guisp={color3}
hi SpellRare cterm=underline gui=underline guifg={base0E} guisp={base0E}

hi csClass ctermfg=3 guifg={color3} cterm=NONE gui=NONE
hi csAttribute ctermfg=3 guifg={color3} cterm=NONE gui=NONE
hi csModifier ctermfg=176 guifg={base0E} cterm=NONE gui=NONE
hi csType ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi csUnspecifiedStatement ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi csContextualStatement ctermfg=176 guifg={base0E} cterm=NONE gui=NONE
hi csNewDecleration ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi cOperator ctermfg=6 guifg={color6} cterm=NONE gui=NONE
hi cPreCondit ctermfg=176 guifg={base0E} cterm=NONE gui=NONE

hi cssColor ctermfg=6 guifg={color6} cterm=NONE gui=NONE
hi cssBraces ctermfg=251 guifg={base05} cterm=NONE gui=NONE
hi cssClassName ctermfg=176 guifg={base0E} cterm=NONE gui=NONE

hi DiffAdd ctermfg=2 guifg={color2} ctermbg=237 guibg={base01} cterm=bold gui=bold 
hi DiffChange ctermfg=243 guifg={base03} ctermbg=237 guibg={base01} cterm=NONE gui=NONE
hi DiffDelete ctermfg=5 guifg={color5} ctermbg=237 guibg={base01} cterm=bold gui=bold
hi DiffText ctermfg=4 guifg={color4} ctermbg=237 guibg={base01} cterm=NONE gui=NONE
hi DiffFile ctermfg=5 guifg={color5} ctermbg=235 guibg={base00} cterm=NONE gui=NONE
hi DiffNewFile ctermfg=2 guifg={color2} ctermbg=235 guibg={base00} cterm=NONE gui=NONE
hi DiffLine ctermfg=4 guifg={color4} ctermbg=235 guibg={base00} cterm=NONE gui=NONE
hi! link DiffAdded DiffAdd
hi! link DiffRemoved DiffDelete
hi! link diffRemoved DiffDelete

hi gitCommitOverflow ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi gitCommitSummary ctermfg=2 guifg={color2} cterm=NONE gui=NONE

hi htmlBold ctermfg=3 guifg={color3} cterm=bold gui=bold 
hi htmlItalic ctermfg=176 guifg={base0E} cterm=italic gui=italic 
hi htmlTag ctermfg=6 guifg={color6} cterm=NONE gui=NONE
hi htmlEndTag ctermfg=6 guifg={color6} cterm=NONE gui=NONE
hi htmlArg ctermfg=3 guifg={color3} cterm=NONE gui=NONE
hi htmlTagName ctermfg=251 guifg={base05} cterm=NONE gui=NONE

hi javaScript ctermfg=251 guifg={base05} cterm=NONE gui=NONE
hi javaScriptNumber ctermfg=1 guifg={color1} cterm=NONE gui=NONE
hi javaScriptBraces ctermfg=251 guifg={base05} cterm=NONE gui=NONE

hi jsonKeyword ctermfg=2 guifg={color2} cterm=NONE gui=NONE
hi jsonQuote ctermfg=2 guifg={color2} cterm=NONE gui=NONE

hi markdownCode ctermfg=2 guifg={color2} cterm=NONE gui=NONE
hi markdownCodeBlock ctermfg=2 guifg={color2} cterm=NONE gui=NONE
hi markdownHeadingDelimiter ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi markdownItalic ctermfg=176 guifg={base0E} cterm=italic gui=italic 
hi markdownBold ctermfg=3 guifg={color3} cterm=bold gui=bold 
hi markdownCodeDelimiter ctermfg=9 guifg={base0F} cterm=italic gui=italic 
hi markdownError ctermfg=251 guifg={base05} ctermbg=235 guibg={base00} cterm=NONE gui=NONE

hi typescriptParens ctermfg=251 guifg={base05} ctermbg=NONE guibg=NONE cterm=NONE gui=NONE

hi NeomakeErrorSign ctermfg=5 guifg={color5} ctermbg=235 guibg={base00} cterm=NONE gui=NONE
hi NeomakeWarningSign ctermfg=3 guifg={color3} ctermbg=235 guibg={base00} cterm=NONE gui=NONE
hi NeomakeInfoSign ctermfg=8 guifg={color8} ctermbg=235 guibg={base00} cterm=NONE gui=NONE
hi NeomakeError ctermfg=5 guifg={color5} cterm=underline gui=underline guisp={color5} 
hi NeomakeWarning ctermfg=5 guifg={color5} cterm=underline gui=underline guisp={color5} 
hi ALEErrorSign ctermfg=5 guifg={color5} ctermbg=235 guibg={base00} cterm=bold gui=bold 
hi ALEWarningSign ctermfg=3 guifg={color3} ctermbg=235 guibg={base00} cterm=bold gui=bold 
hi ALEInfoSign ctermfg=8 guifg={color8} ctermbg=235 guibg={base00} cterm=bold gui=bold 

hi NERDTreeExecFile ctermfg=251 guifg={base05} cterm=NONE gui=NONE
hi NERDTreeDirSlash ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi NERDTreeOpenable ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi NERDTreeFile ctermfg=NONE guifg=NONE cterm=NONE gui=NONE
hi NERDTreeFlags ctermfg=4 guifg={color4} cterm=NONE gui=NONE

hi phpComparison ctermfg=251 guifg={base05} cterm=NONE gui=NONE
hi phpParent ctermfg=251 guifg={base05} cterm=NONE gui=NONE
hi phpMemberSelector ctermfg=251 guifg={base05} cterm=NONE gui=NONE

hi pythonRepeat ctermfg=176 guifg={base0E} cterm=NONE gui=NONE
hi pythonOperator ctermfg=176 guifg={base0E} cterm=NONE gui=NONE

hi rubyConstant ctermfg=3 guifg={color3} cterm=NONE gui=NONE
hi rubySymbol ctermfg=2 guifg={color2} cterm=NONE gui=NONE
hi rubyAttribute ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi rubyInterpolation ctermfg=2 guifg={color2} cterm=NONE gui=NONE
hi rubyInterpolationDelimiter ctermfg=9 guifg={base0F} cterm=NONE gui=NONE
hi rubyStringDelimiter ctermfg=2 guifg={color2} cterm=NONE gui=NONE
hi rubyRegexp ctermfg=6 guifg={color6} cterm=NONE gui=NONE

hi sassidChar ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi sassClassChar ctermfg=1 guifg={color1} cterm=NONE gui=NONE
hi sassInclude ctermfg=176 guifg={base0E} cterm=NONE gui=NONE
hi sassMixing ctermfg=176 guifg={base0E} cterm=NONE gui=NONE
hi sassMixinName ctermfg=4 guifg={color4} cterm=NONE gui=NONE

hi vimfilerLeaf ctermfg=251 guifg={base05} cterm=NONE gui=NONE
hi vimfilerNormalFile ctermfg=251 guifg={base05} ctermbg=235 guibg={base00} cterm=NONE gui=NONE
hi vimfilerOpenedFile ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi vimfilerClosedFile ctermfg=4 guifg={color4} cterm=NONE gui=NONE

hi GitGutterAdd ctermfg=2 guifg={color2} ctermbg=235 guibg={base00} cterm=bold gui=bold 
hi GitGutterChange ctermfg=4 guifg={color4} ctermbg=235 guibg={base00} cterm=bold gui=bold 
hi GitGutterDelete ctermfg=5 guifg={color5} ctermbg=235 guibg={base00} cterm=bold gui=bold 
hi GitGutterChangeDelete ctermfg=176 guifg={base0E} ctermbg=235 guibg={base00} cterm=bold gui=bold 

hi SignifySignAdd ctermfg=2 guifg={color2} ctermbg=235 guibg={base00} cterm=bold gui=bold 
hi SignifySignChange ctermfg=4 guifg={color4} ctermbg=235 guibg={base00} cterm=bold gui=bold 
hi SignifySignDelete ctermfg=5 guifg={color5} ctermbg=235 guibg={base00} cterm=bold gui=bold 
hi SignifySignChangeDelete ctermfg=176 guifg={base0E} ctermbg=235 guibg={base00} cterm=bold gui=bold 
hi SignifySignDeleteFirstLine ctermfg=5 guifg={color5} ctermbg=235 guibg={base00} cterm=bold gui=bold 

hi xmlTag ctermfg=6 guifg={color6} cterm=NONE gui=NONE
hi xmlTagName ctermfg=251 guifg={base05} cterm=NONE gui=NONE
hi xmlEndTag ctermfg=6 guifg={color6} cterm=NONE gui=NONE
hi Defx_filename_directory ctermfg=4 guifg={color4} cterm=NONE gui=NONE

hi TelescopeBorder ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi TelescopePromptBorder ctermfg=6 guifg={color6} cterm=NONE gui=NONE

hi CocErrorSign ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi CocWarningSign ctermfg=3 guifg={color3} cterm=NONE gui=NONE
hi CocInfoSign ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi CocHintSign ctermfg=6 guifg={color6} cterm=NONE gui=NONE
hi CocErrorFloat ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi CocWarningFloat ctermfg=3 guifg={color3} cterm=NONE gui=NONE
hi CocInfoFloat ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi CocHintFloat ctermfg=6 guifg={color6} cterm=NONE gui=NONE
hi CocDiagnosticsError ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi CocDiagnosticsWarning ctermfg=3 guifg={color3} cterm=NONE gui=NONE
hi CocDiagnosticsInfo ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi CocDiagnosticsHint ctermfg=6 guifg={color6} cterm=NONE gui=NONE
hi CocSelectedText ctermfg=176 guifg={base0E} cterm=NONE gui=NONE
hi CocCodeLens ctermfg=145 guifg={base04} cterm=NONE gui=NONE

hi semshiLocal ctermfg=9 guifg={base0F} cterm=NONE gui=NONE
hi semshiGlobal ctermfg=1 guifg={color1} cterm=NONE gui=NONE
hi semshiImported ctermfg=9 guifg={base0F} cterm=bold gui=bold
hi semshiParameter ctermfg=4 guifg={color4} cterm=NONE gui=NONE
hi semshiParameterUnused ctermfg=12 guifg={color12} cterm=underline gui=underline
hi semshiFree ctermfg=5 guifg={color5} cterm=NONE gui=NONE
hi semshiBuiltin ctermfg=13 guifg={color13} cterm=NONE gui=NONE
hi semshiAttribute ctermfg=6 guifg={color6} cterm=NONE gui=NONE
hi semshiSelf ctermfg=145 guifg={base04} cterm=NONE gui=NONE
hi semshiUnresolved ctermfg=3 guifg={color3} cterm=underline gui=underline
hi semshiSelected ctermfg=235 guifg={base00} guibg=#ff875f ctermbg=209 cterm=NONE gui=NONE
hi semshiErrorSign ctermfg=251 guifg={base05} guibg=#d70000 ctermbg=160 cterm=NONE gui=NONE
hi semshiErrorChar ctermfg=251 guifg={base05} guibg=#d70000 ctermbg=160 cterm=NONE gui=NONE

hi StatusLine ctermfg=7 guifg={color7} ctermbg=8 guibg={color8} cterm=NONE gui=NONE
hi StatusLineNC ctermfg=240 guifg={base02} ctermbg=8 guibg={color8} cterm=NONE gui=NONE
hi StatusLineTerm ctermfg=10 guifg={color10} ctermbg=2 guibg={color2} cterm=NONE gui=NONE
hi StatusLineTermNC ctermfg=11 guifg={color11} ctermbg=237 guibg={base01} cterm=NONE gui=NONE
hi! link User Normal
