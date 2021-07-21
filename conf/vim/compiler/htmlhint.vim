if exists('current_compiler')
  finish
endif
let current_compiler = 'htmlhint'

if exists(':CompilerSet') != 2		" older Vim always used :setlocal
  command -nargs=* CompilerSet setlocal <args>
endif

CompilerSet makeprg=htmlhint\ --format=unix\ --nocolor\ %
CompilerSet errorformat=%f:%l:%c:\ %m,%-G%.%#
