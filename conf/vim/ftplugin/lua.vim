if has_key(g:, 'ale_enabled')
  call ale#fix#registry#Add('luaformat', 'ale#fixers#lua_format#Fix', ['lua'], 'Formatter for ale')
  call ale#fix#registry#Add('prettier_lua', 'ale#fixers#prettier_lua#ApplyFixForVersion', ['lua'], 'Formatter for ale')
endif
let g:ale_lua_luacheck_options = '-d'
let b:ale_fixers = ['prettier_lua']
set formatprg=prettier\ --parser=lua
