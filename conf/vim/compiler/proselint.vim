if exists('current_compiler')
  finish
endif
let current_compiler = 'proselint'

let s:cpo_save = &cpoptions
set cpoptions-=C

CompilerSet makeprg=proselint\ %
CompilerSet errorformat=%f:%l:%c:%m

let &cpoptions = s:cpo_save
unlet s:cpo_save
