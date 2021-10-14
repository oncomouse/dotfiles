function! vsnip_completefunc#completefunc(findstart, base) abort
	return luaeval('require("vsnip-completefunc")(_A[1], _A[2])', [a:findstart, a:base])
endfunction
