if exists('current_compiler')
  finish
endif
let current_compiler = 'vale'

if exists(':CompilerSet') != 2		" older Vim always used :setlocal
  command -nargs=* CompilerSet setlocal <args>
endif

CompilerSet makeprg=markdown_combo_linter
CompilerSet errorformat=%f:%l:%c:%*[^:]:%m

