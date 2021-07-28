function! bibrefomni#omnifunc(findstart, base) abort
    return luaeval('require("bibrefomni").omnifunc(_A[1], _A[2])', [a:findstart, a:base])
endfunction
