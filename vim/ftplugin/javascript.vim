" Just disable tsserver:
let b:ale_fixers = [function('ale#fixers#semistandard#Fix')]

function s:get_makeprg()
  let &l:makeprg = 'npx semistandard --no-install'
  set errorformat+=%f:\ line\ %l\\,\ col\ %c\\,\ %m,%-G%.%#
endfunction
