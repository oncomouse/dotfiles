if exists('current_compiler')
  finish
endif
let current_compiler = 'luacheck'

if exists(':CompilerSet') != 2		" older Vim always used :setlocal
  command -nargs=* CompilerSet setlocal <args>
endif

CompilerSet makeprg=luacheck\ --formatter\ visual_studio\ --globals\ vim\ -d\ --read-globals\ after_each\ before_each\ it\ packer_plugins\ vimp\ --
CompilerSet errorformat=%f(%l\\,%c)\ :\ %t%*[^\:]:\ %m
