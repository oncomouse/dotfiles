local M = {}
function M.setup ()
	local function make_endwise_rule(at, ed, ft, syn)
		require("lexima-insx").add_rule({
			char = "<CR>",
			input = "<CR>",
			input_after = "<CR>" .. ed,
			at = at,
			except = [[\C\v^(\s*)\S.*\%#\n%(%(\s*|\1\s.+)\n)*\1]] .. ed,
			filetype = ft,
			syntax = syn,
		})
	end

	for _, at in pairs({
		"fu",
		"fun",
		"func",
		"funct",
		"functi",
		"functio",
		"function",
		"if",
		"wh",
		"whi",
		"whil",
		"while",
		"for",
		"try",
		"def",
	}) do
		make_endwise_rule([[^\s*]] .. at .. [[\>.*\%#$]], "end" .. at, "vim", {})
	end

	for _, at in pairs({ "aug", "augroup" }) do
		make_endwise_rule([[^\s*]] .. at .. [[\s\+.\+\%#$]], at .. " END", "vim", {})
	end

	-- ruby
	make_endwise_rule(
		[[^\s*\%(module\|def\|class\|if\|unless\|for\|while\|until\|case\)\>\%(.*[^.:@$]\<end\>\)\@!.*\%#$]],
		"end",
		"ruby",
		{}
	)
	make_endwise_rule([[^\s*\%(begin\)\s*\%#$]], "end", "ruby", {})
	make_endwise_rule([[\%(^\s*#.*\)\@<!do\%(\s*|.*|\)\?\s*\%#$]], "end", "ruby", {})
	make_endwise_rule([[\<\%(if\|unless\)\>.*\%#$]], "end", "ruby", "rubyConditionalExpression")

	-- elixir
	make_endwise_rule([[\%(^\s*#.*\)\@<!do\s*\%#$]], "end", "elixir", {})

	-- sh
	make_endwise_rule([[^\s*if\>.*\%#$]], "fi", { "sh", "zsh" }, {})
	make_endwise_rule([[^\s*case\>.*\%#$]], "esac", { "sh", "zsh" }, {})
	make_endwise_rule([[\%(^\s*#.*\)\@<!do\>.*\%#$]], "done", { "sh", "zsh" }, {})

	-- julia
	make_endwise_rule(
		[[\%(^\s*#.*\)\@<!\<\%(module\|struct\|function\|if\|for\|while\|do\|let\|macro\)\>\%(.*\<end\>\)\@!.*\%#$]],
		"end",
		"julia",
		{}
	)
	make_endwise_rule([[\%(^\s*#.*\)\@<!\s*\<\%(begin\|try\|quote\)\s*\%#$]], "end", "julia", {})

	-- Lua endwise rules:
	make_endwise_rule([[^\s*if\>.*then\%(.*[^.:@$]\<end\>\)\@!.*\%#]], "end", "lua", {})
	make_endwise_rule([[^\s*\%(for\|while\)\>.*do\%(.*[^.:@$]\<end\>\)\@!.*\%#]], "end", "lua", {})
	make_endwise_rule([[^\s*\%(local\)\=.*function\>\%(.*[^.:@$]\<end\>\)\@!.*\%#]], "end", "lua", {})
end

return M
