function! bibliography_omnifunc#omnifunc(findstart, base) abort
    return luaeval('require("bibliography_omnifunc").omnifunc(_A[1], _A[2])', [a:findstart, a:base])
endfunction
