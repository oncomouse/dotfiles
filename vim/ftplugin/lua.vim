" npm i -g gakada/plugin-lua#prettier2
" Until they fix plugin-lua for prettier2
let b:ale_fixers = [function('ale#fixers#prettier_lua#ApplyFixForVersion')]
