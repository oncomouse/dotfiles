" Just disable tsserver:
let b:ale_linters = ['eslint', 'fecs', 'flow', 'jscs', 'jshint', 'standard', 'xo']
let b:ale_fixers = [function('ale#fixers#semistandard#Fix')]
