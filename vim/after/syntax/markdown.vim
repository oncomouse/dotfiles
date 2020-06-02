syn match mkdPandocCitation /\v\[[^\@]*\@[_:.#$%&\-+?<>~\/a-zA-Z0-9]+[^\]]+\]/ contains=mkdPandocCiteKey,mkdBold,mkdItalic,htmlBold,htmlItalic
syn match mkdPandocCiteKey /\v\@[_:.#$%&\-+?<>~\/a-zA-Z0-9]+/ contained
syn cluster mkdNonListItem add=mkdPandocCitation,mkdPandocCiteKey
syn match mmkdDelimiterkdDelimiter /\v[\[\]\(\)\*\_]/
hi link mkdPandocCiteKey Tag
hi mkdDelimiter ctermfg=8 guifg=#65737E
