syn match mkdPandocCitation /\v\[[^\@]*\@[_:.#$%&\-+?<>~\/a-zA-Z0-9]+[^\]]+\]/ contains=mkdPandocCiteKey,mkdBold,mkdItalic,htmlBold,htmlItalic
syn match mkdPandocCiteKey /\v\@[_:.#$%&\-+?<>~\/a-zA-Z0-9]+/ contained
syn cluster mkdNonListItem add=mkdPandocCitation,mkdPandocCiteKey
hi link mkdPandocCiteKey Tag
hi mkdPandocCitation ctermfg=8 guifg=#65737E
" Add custom contains for blockquote:
syn region mkdBlockquote   start=/^\s*>/                   end=/$/ contains=mkdLink,mkdInlineURL,mkdLineBreak,@Spell,mkdPandocCitation,mkdMath
