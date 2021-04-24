if exists('current_compiler')
  finish
endif
let current_compiler = 'vale'

let s:cpo_save = &cpoptions
set cpoptions-=C

CompilerSet makeprg=vale\ --output\ line\ %
CompilerSet errorformat=%f:%l:%c:%m

let &cpoptions = s:cpo_save
unlet s:cpo_save
