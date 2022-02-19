_dotfiles = _dotfiles or {}
local function make_rule(at, ed, ft, syn)
	return {
		char = "<CR>",
		input = "<CR>",
		input_after = "<CR>" .. ed,
		at = at,
		except = [[\C\v^(\s*)\S.*%#\n%(%(\s*|\1\s.+)\n)*\1]] .. ed,
		filetype = ft,
		syntax = syn
	}
end
_dotfiles.lua_endwise = function()
	vim.fn["lexima#add_rule"](make_rule([[^\s*if\>.*then\%(.*[^.:@$]\<end\>\)\@!.*\%#]], "end", "lua", {}))
	vim.fn["lexima#add_rule"](make_rule([[^\s*\%(for\|while\)\>.*do\%(.*[^.:@$]\<end\>\)\@!.*\%#]], "end", "lua", {}))
	vim.fn["lexima#add_rule"](make_rule([[^\s*\%(local\)\=.*function\>\%(.*[^.:@$]\<end\>\)\@!.*\%#]], "end", "lua", {}))
end
