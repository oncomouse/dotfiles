set background=dark
highlight! clear
if exists('syntax_on')
        syntax reset
endif
let g:colors_name = 'wal'
hi! Bold cterm=bold gui=bold 
hi! Debug ctermfg=5 guifg={color5} 
hi! Directory ctermfg=4 guifg={color4} 
hi! ErrorMsg ctermfg=5 guifg={color5} ctermbg=235 guibg={base00} 
hi! Exception ctermfg=5 guifg={color5} 
hi! FoldColumn ctermfg=4 guifg={color4} ctermbg=235 guibg={base00} 
hi! Folded ctermfg=145 guifg={base04} ctermbg=237 guibg={base01} cterm=italic gui=italic 
hi! IncSearch ctermfg=237 guifg={base01} ctermbg=1 guibg={color1} cterm=NONE gui=NONE 
hi! Italic cterm=italic gui=italic 

hi! Macro ctermfg=5 guifg={color5} 
hi! MatchParen ctermfg=251 guifg={base05} ctermbg=243 guibg={base03} 
hi! ModeMsg ctermfg=2 guifg={color2} 
hi! MoreMsg ctermfg=2 guifg={color2} 
hi! Question ctermfg=4 guifg={color4} 
hi! Search ctermfg=243 guifg={base03} ctermbg=3 guibg={color3} 
hi! SpecialKey ctermfg=243 guifg={base03} 
hi! TooLong ctermfg=5 guifg={color5} 
hi! Underlined ctermfg=5 guifg={color5} 
hi! Visual ctermbg=240 guibg={base02} ctermfg=235 guifg={base00} 
hi! VisualNOS ctermfg=5 guifg={color5} 
hi! WarningMsg ctermfg=5 guifg={color5} 
hi! WildMenu ctermfg=251 guifg={base05} ctermbg=4 guibg={color4} 
hi! Title ctermfg=4 guifg={color4} cterm=bold gui=bold 
hi! Conceal ctermfg=237 guifg={base01} ctermbg=235 guibg={base00} 
hi! Cursor ctermfg=235 guifg={base00} ctermbg=251 guibg={base05} 
hi! NonText ctermfg=243 guifg={base03} 
hi! Normal ctermfg=251 guifg={base05} ctermbg=235 guibg={base00} 
hi! EndOfBuffer ctermfg=251 guifg={base05} ctermbg=235 guibg={base00} 
hi! LineNr ctermfg=243 guifg={base03} ctermbg=235 guibg={base00} 
hi! SignColumn ctermfg=240 guifg={base02} ctermbg=235 guibg={base00} 
hi! StatusLine ctermfg=237 guifg={base01} ctermbg=243 guibg={base03} 
hi! StatusLineNC ctermfg=243 guifg={base03} ctermbg=237 guibg={base01} 
hi! VertSplit ctermfg=235 guifg={base00} ctermbg=240 guibg={base02} 
hi! ColorColumn ctermfg=240 guifg={base02} 
hi! CursorColumn ctermfg=240 guifg={base02} 
hi! CursorLine ctermfg=237 guifg={base01} cterm=None gui=None 
hi! CursorLineNr ctermfg=145 guifg={base04} ctermbg=235 guibg={base00} cterm=None gui=None
hi! PMenu ctermfg=145 guifg={base04} ctermbg=237 guibg={base01} 
hi! PMenuSel ctermfg=251 guifg={base05} ctermbg=4 guibg={color4} 
hi! PmenuSbar ctermfg=240 guifg={base02} 
hi! PmenuThumb ctermfg=251 guifg={base05} 
hi! TabLine ctermfg=243 guifg={base03} ctermbg=237 guibg={base01} 
hi! TabLineFill ctermfg=243 guifg={base03} ctermbg=237 guibg={base01} 
hi! TabLineSel ctermfg=2 guifg={color2} ctermbg=237 guibg={base01} 
hi! helpExample ctermfg=3 guifg={color3} 
hi! helpCommand ctermfg=3 guifg={color3} 

hi! Boolean ctermfg=1 guifg={color1} 
hi! Character ctermfg=5 guifg={color5} 
hi! Comment ctermfg=145 guifg={base04} cterm=italic gui=italic 
hi! Conditional ctermfg=176 guifg={base0E} 
hi! Constant ctermfg=1 guifg={color1} 
hi! Define ctermfg=176 guifg={base0E} 
hi! Delimiter ctermfg=9 guifg={base0F} 
hi! Float ctermfg=1 guifg={color1} 
hi! Function ctermfg=4 guifg={color4} 

hi! Identifier ctermfg=6 guifg={color6} 
hi! Include ctermfg=4 guifg={color4} 
hi! Keyword ctermfg=176 guifg={base0E} 

hi! Label ctermfg=3 guifg={color3} 
hi! Number ctermfg=1 guifg={color1} 
hi! Operator ctermfg=251 guifg={base05} 
hi! PreProc ctermfg=3 guifg={color3} 
hi! Repeat ctermfg=3 guifg={color3} 
hi! Special ctermfg=6 guifg={color6} 
hi! SpecialChar ctermfg=9 guifg={base0F} 
hi! Statement ctermfg=5 guifg={color5} 
hi! StorageClass ctermfg=3 guifg={color3} 
hi! String ctermfg=2 guifg={color2} 
hi! Structure ctermfg=176 guifg={base0E} 
hi! Tag ctermfg=3 guifg={color3} 
hi! Todo ctermfg=3 guifg={color3} ctermbg=237 guibg={base01} 
hi! Type ctermfg=3 guifg={color3} 
hi! Typedef ctermfg=3 guifg={color3} 

" hi! LspDiagnosticsDefaultError  
hi! LspDiagnosticsSignError ctermfg=5 guifg={color5} 
hi! LspDiagnosticsUnderlineError cterm=undercurl gui=undercurl 

" hi! LspDiagnosticsDefaultWarning  
hi! LspDiagnosticsSignWarning ctermfg=3 guifg={color3} 
hi! LspDiagnosticsUnderlineWarning cterm=undercurl gui=undercurl 

" hi! LspDiagnosticsDefaultInformation  
hi! LspDiagnosticsSignInformation ctermfg=4 guifg={color4} 
hi! LspDiagnosticsUnderlineInformation cterm=undercurl gui=undercurl 

" hi! LspDiagnosticsDefaultHint  
hi! LspDiagnosticsSignHint ctermfg=6 guifg={color6} 
hi! LspDiagnosticsUnderlineHint cterm=undercurl gui=undercurl 

hi! TSAnnotation ctermfg=4 guifg={color4} 
hi! TSAttribute ctermfg=4 guifg={color4} 
hi! TSBoolean ctermfg=4 guifg={color4} 
hi! TSCharacter ctermfg=3 guifg={color3} 
hi! TSComment ctermfg=145 guifg={base04} cterm=italic gui=italic 
hi! TSConditional ctermfg=5 guifg={color5} 
hi! TSConstBuiltin ctermfg=1 guifg={color1} 
hi! TSConstMacro ctermfg=1 guifg={color1} 
hi! TSConstant ctermfg=1 guifg={color1} 
hi! TSConstructor ctermfg=05 guifg={base05} 
hi! TSEmphasis cterm=bold gui=bold 
hi! TSError ctermfg=5 guifg={color5} 
hi! TSException ctermfg=5 guifg={color5} 
hi! TSField ctermfg=2 guifg={color2} 
hi! TSFloat ctermfg=4 guifg={color4} 
hi! TSFuncBuiltin ctermfg=1 guifg={color1} 
hi! TSFuncMacro ctermfg=1 guifg={color1} 
hi! TSFunction ctermfg=4 guifg={color4} 
hi! TSInclude ctermfg=6 guifg={color6} 
hi! TSKeyword ctermfg=0E guifg={base0E} 
hi! TSKeywordFunction ctermfg=6 guifg={color6} 
hi! TSKeywordOperator ctermfg=0E guifg={base0E} 
hi! TSLabel ctermfg=6 guifg={color6} 
hi! TSMethod ctermfg=4 guifg={color4} 
hi! TSNamespace ctermfg=4 guifg={color4} 
hi! TSNumber ctermfg=4 guifg={color4} 
hi! TSOperator ctermfg=05 guifg={base05} 
hi! TSParameter ctermfg=3 guifg={color3} 
hi! TSParameterReference ctermfg=1 guifg={color1} 
hi! TSProperty ctermfg=3 guifg={color3} 
hi! TSPunctBracket ctermfg=05 guifg={base05} 
hi! TSPunctDelimiter ctermfg=05 guifg={base05} 
hi! TSPunctSpecial ctermfg=05 guifg={base05} 
hi! TSRepeat ctermfg=5 guifg={color5} 
hi! TSString ctermfg=4 guifg={color4} 
hi! TSStringEscape ctermfg=2 guifg={color2} 
hi! TSStringRegex ctermfg=2 guifg={color2} 
hi! TSStructure ctermfg=4 guifg={color4} 
hi! TSTag ctermfg=3 guifg={color3} 
hi! TSTagDelimiter ctermfg=6 guifg={color6} 
hi! TSText ctermfg=2 guifg={color2} 
hi! TSType ctermfg=4 guifg={color4} 
hi! TSTypeBuiltin ctermfg=4 guifg={color4} 
hi! TSURI cterm=underline gui=underline ctermbg=237 guibg={base01} 
hi! TSUnderline cterm=underline gui=underline
hi! TSVariable ctermfg=3 guifg={color3} 
hi! TSVariableBuiltin ctermfg=5 guifg={color5} 

hi! SpellBad cterm=underline gui=underline ctermbg=237 guibg={base01} guisp={color5} 
hi! SpellLocal cterm=underline gui=underline ctermbg=237 guibg={base01} guisp={color6} 
hi! SpellCap cterm=underline gui=underline ctermbg=237 guibg={base01} guisp={color3} 
hi! SpellRare cterm=underline gui=underline ctermbg=237 guibg={base01} guisp={base0E} 

hi! csClass ctermfg=3 guifg={color3} 
hi! csAttribute ctermfg=3 guifg={color3} 
hi! csModifier ctermfg=176 guifg={base0E} 
hi! csType ctermfg=5 guifg={color5} 
hi! csUnspecifiedStatement ctermfg=4 guifg={color4} 
hi! csContextualStatement ctermfg=176 guifg={base0E} 
hi! csNewDecleration ctermfg=5 guifg={color5} 
hi! cOperator ctermfg=6 guifg={color6} 
hi! cPreCondit ctermfg=176 guifg={base0E} 

hi! cssColor ctermfg=6 guifg={color6} 
hi! cssBraces ctermfg=251 guifg={base05} 
hi! cssClassName ctermfg=176 guifg={base0E} 

hi! DiffAdd ctermfg=2 guifg={color2} ctermbg=237 guibg={base01} cterm=bold gui=bold 
hi! DiffChange ctermfg=243 guifg={base03} ctermbg=237 guibg={base01} 
hi! DiffDelete ctermfg=5 guifg={color5} ctermbg=237 guibg={base01} 
hi! DiffText ctermfg=4 guifg={color4} ctermbg=237 guibg={base01} 
hi! DiffAdded ctermfg=251 guifg={base05} ctermbg=2 guibg={color2} cterm=bold gui=bold 
hi! DiffFile ctermfg=5 guifg={color5} ctermbg=235 guibg={base00} 
hi! DiffNewFile ctermfg=2 guifg={color2} ctermbg=235 guibg={base00} 
hi! DiffLine ctermfg=4 guifg={color4} ctermbg=235 guibg={base00} 
hi! DiffRemoved ctermfg=251 guifg={base05} ctermbg=5 guibg={color5} cterm=bold gui=bold 

hi! gitCommitOverflow ctermfg=5 guifg={color5} 
hi! gitCommitSummary ctermfg=2 guifg={color2} 

hi! htmlBold ctermfg=3 guifg={color3} cterm=bold gui=bold 
hi! htmlItalic ctermfg=176 guifg={base0E} cterm=italic gui=italic 
hi! htmlTag ctermfg=6 guifg={color6} 
hi! htmlEndTag ctermfg=6 guifg={color6} 
hi! htmlArg ctermfg=3 guifg={color3} 
hi! htmlTagName ctermfg=251 guifg={base05} 

hi! javaScript ctermfg=251 guifg={base05} 
hi! javaScriptNumber ctermfg=1 guifg={color1} 
hi! javaScriptBraces ctermfg=251 guifg={base05} 

hi! jsonKeyword ctermfg=2 guifg={color2} 
hi! jsonQuote ctermfg=2 guifg={color2} 

hi! markdownCode ctermfg=2 guifg={color2} 
hi! markdownCodeBlock ctermfg=2 guifg={color2} 
hi! markdownHeadingDelimiter ctermfg=4 guifg={color4} 
hi! markdownItalic ctermfg=176 guifg={base0E} cterm=italic gui=italic 
hi! markdownBold ctermfg=3 guifg={color3} cterm=bold gui=bold 
hi! markdownCodeDelimiter ctermfg=9 guifg={base0F} cterm=italic gui=italic 
hi! markdownError ctermfg=251 guifg={base05} ctermbg=235 guibg={base00} 

hi! typescriptParens ctermfg=251 guifg={base05} ctermbg=NONE guibg=NONE 

hi! NeomakeErrorSign ctermfg=5 guifg={color5} ctermbg=235 guibg={base00} 
hi! NeomakeWarningSign ctermfg=3 guifg={color3} ctermbg=235 guibg={base00} 
hi! NeomakeInfoSign ctermfg=8 guifg={color8} ctermbg=235 guibg={base00} 
hi! NeomakeError ctermfg=5 guifg={color5} cterm=underline gui=underline guisp={color5} 
hi! NeomakeWarning ctermfg=5 guifg={color5} cterm=underline gui=underline guisp={color5} 
hi! ALEErrorSign ctermfg=5 guifg={color5} ctermbg=235 guibg={base00} cterm=bold gui=bold 
hi! ALEWarningSign ctermfg=3 guifg={color3} ctermbg=235 guibg={base00} cterm=bold gui=bold 
hi! ALEInfoSign ctermfg=8 guifg={color8} ctermbg=235 guibg={base00} cterm=bold gui=bold 

hi! NERDTreeExecFile ctermfg=251 guifg={base05} 
hi! NERDTreeDirSlash ctermfg=4 guifg={color4} 
hi! NERDTreeOpenable ctermfg=4 guifg={color4} 
hi! NERDTreeFile ctermfg=NONE guifg=NONE 
hi! NERDTreeFlags ctermfg=4 guifg={color4} 

hi! phpComparison ctermfg=251 guifg={base05} 
hi! phpParent ctermfg=251 guifg={base05} 
hi! phpMemberSelector ctermfg=251 guifg={base05} 

hi! pythonRepeat ctermfg=176 guifg={base0E} 
hi! pythonOperator ctermfg=176 guifg={base0E} 

hi! rubyConstant ctermfg=3 guifg={color3} 
hi! rubySymbol ctermfg=2 guifg={color2} 
hi! rubyAttribute ctermfg=4 guifg={color4} 
hi! rubyInterpolation ctermfg=2 guifg={color2} 
hi! rubyInterpolationDelimiter ctermfg=9 guifg={base0F} 
hi! rubyStringDelimiter ctermfg=2 guifg={color2} 
hi! rubyRegexp ctermfg=6 guifg={color6} 

hi! sassidChar ctermfg=5 guifg={color5} 
hi! sassClassChar ctermfg=1 guifg={color1} 
hi! sassInclude ctermfg=176 guifg={base0E} 
hi! sassMixing ctermfg=176 guifg={base0E} 
hi! sassMixinName ctermfg=4 guifg={color4} 

hi! vimfilerLeaf ctermfg=251 guifg={base05} 
hi! vimfilerNormalFile ctermfg=251 guifg={base05} ctermbg=235 guibg={base00} 
hi! vimfilerOpenedFile ctermfg=4 guifg={color4} 
hi! vimfilerClosedFile ctermfg=4 guifg={color4} 

hi! GitGutterAdd ctermfg=2 guifg={color2} ctermbg=235 guibg={base00} cterm=bold gui=bold 
hi! GitGutterChange ctermfg=4 guifg={color4} ctermbg=235 guibg={base00} cterm=bold gui=bold 
hi! GitGutterDelete ctermfg=5 guifg={color5} ctermbg=235 guibg={base00} cterm=bold gui=bold 
hi! GitGutterChangeDelete ctermfg=176 guifg={base0E} ctermbg=235 guibg={base00} cterm=bold gui=bold 

hi! SignifySignAdd ctermfg=2 guifg={color2} ctermbg=235 guibg={base00} cterm=bold gui=bold 
hi! SignifySignChange ctermfg=4 guifg={color4} ctermbg=235 guibg={base00} cterm=bold gui=bold 
hi! SignifySignDelete ctermfg=5 guifg={color5} ctermbg=235 guibg={base00} cterm=bold gui=bold 
hi! SignifySignChangeDelete ctermfg=176 guifg={base0E} ctermbg=235 guibg={base00} cterm=bold gui=bold 
hi! SignifySignDeleteFirstLine ctermfg=5 guifg={color5} ctermbg=235 guibg={base00} cterm=bold gui=bold 

hi! xmlTag ctermfg=6 guifg={color6} 
hi! xmlTagName ctermfg=251 guifg={base05} 
hi! xmlEndTag ctermfg=6 guifg={color6} 
hi! Defx_filename_directory ctermfg=4 guifg={color4} 

hi! TelescopeBorder ctermfg=4 guifg={color4} 
hi! TelescopePromptBorder ctermfg=6 guifg={color6} 

hi! CocErrorSign ctermfg=5 guifg={color5} 
hi! CocWarningSign ctermfg=3 guifg={color3} 
hi! CocInfoSign ctermfg=4 guifg={color4} 
hi! CocHintSign ctermfg=6 guifg={color6} 
hi! CocErrorFloat ctermfg=5 guifg={color5} 
hi! CocWarningFloat ctermfg=3 guifg={color3} 
hi! CocInfoFloat ctermfg=4 guifg={color4} 
hi! CocHintFloat ctermfg=6 guifg={color6} 
hi! CocDiagnosticsError ctermfg=5 guifg={color5} 
hi! CocDiagnosticsWarning ctermfg=3 guifg={color3} 
hi! CocDiagnosticsInfo ctermfg=4 guifg={color4} 
hi! CocDiagnosticsHint ctermfg=6 guifg={color6} 
hi! CocSelectedText ctermfg=176 guifg={base0E} 
hi! CocCodeLens ctermfg=145 guifg={base04} 

hi! semshiLocal ctermfg=9 guifg={base0F}
hi! semshiGlobal ctermfg=1 guifg={color1}
hi! semshiImported ctermfg=9 guifg={base0F} cterm=bold gui=bold
hi! semshiParameter ctermfg=4 guifg={color4}
hi! semshiParameterUnused ctermfg=12 guifg={color12} cterm=underline gui=underline
hi! semshiFree ctermfg=5 guifg={color5}
hi! semshiBuiltin ctermfg=13 guifg={color13}
hi! semshiAttribute ctermfg=6 guifg={color6}
hi! semshiSelf ctermfg=145 guifg={base04}
hi! semshiUnresolved ctermfg=3 guifg={color3} cterm=underline gui=underline
hi! semshiSelected ctermfg=235 guifg={base00} guibg=#ff875f ctermbg=209
hi! semshiErrorSign ctermfg=251 guifg={base05} guibg=#d70000 ctermbg=160
hi! semshiErrorChar ctermfg=251 guifg={base05} guibg=#d70000 ctermbg=160

hi! IndentBlanklineChar ctermfg=237 guifg={base01} ctermbg=235 guibg={base00} cterm=nocombine gui=nocombine 

hi! VisualMode ctermfg=235 guifg={base00} ctermbg=9 guibg={color9} cterm=bold gui=bold  
hi! InsertMode ctermfg=235 guifg={base00} ctermbg=2 guibg={color2} cterm=bold gui=bold  
hi! ReplaceMode ctermfg=235 guifg={base00} ctermbg=13 guibg={color13} cterm=bold gui=bold  
hi! CommandMode ctermfg=235 guifg={base00} ctermbg=6 guibg={color6} cterm=bold gui=bold  
hi! TerminalMode ctermfg=235 guifg={base00} ctermbg=176 guibg={base0E} cterm=bold gui=bold  
hi! NormalMode ctermfg=235 guifg={base00} ctermbg=4 guibg={color4} cterm=bold gui=bold  
hi! VisualModeInv ctermfg=9 guifg={color9} ctermbg=235 guibg={base00} 
hi! InsertModeInv ctermfg=2 guifg={color2} ctermbg=235 guibg={base00} 
hi! ReplaceModeInv ctermfg=13 guifg={color13} ctermbg=235 guibg={base00} 
hi! CommandModeInv ctermfg=6 guifg={color6} ctermbg=235 guibg={base00} 
hi! TerminalModeInv ctermfg=176 guifg={base0E} ctermbg=235 guibg={base00} 
hi! NormalModeInv ctermfg=4 guifg={color4} ctermbg=235 guibg={base00} 
hi! StatusLine ctermfg=237 guifg={base01} 
hi! StatusLineNC ctermfg=11 guifg={color11} ctermbg=237 guibg={base01} 
hi! StatusLineTerm ctermfg=10 guifg={color10} ctermbg=2 guibg={color2} 
hi! StatusLineTermNC ctermfg=11 guifg={color11} ctermbg=237 guibg={base01} 
hi! User1 ctermfg=145 guifg={base04} ctermbg=237 guibg={base01} 
hi! link User Normal 
hi! StatusWarning ctermfg=237 guifg={base01} ctermbg=3 guibg={color3} 
hi! StatusError ctermfg=237 guifg={base01} ctermbg=1 guibg={color1} 
hi! StatusOk ctermfg=251 guifg={base05} ctermbg=237 guibg={base01} 
hi! StatusWarningInv ctermfg=3 guifg={color3} ctermbg=235 guibg={base00}  
hi! StatusErrorInv ctermfg=1 guifg={color1} ctermbg=235 guibg={base00}  
hi! StatusOkInv ctermfg=237 guifg={base01} ctermbg=235 guibg={base00}  
hi! StatusInfoInv ctermfg=237 guifg={base01} ctermbg=235 guibg={base00} 
hi! StatusLineInfo ctermfg=251 guifg={base05} ctermbg=240 guibg={base02} 
hi! StatusLineInfoInv ctermfg=240 guifg={base02} ctermbg=235 guibg={base00} 
