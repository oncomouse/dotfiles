vim.keymap.set({ "n", "i" }, "<C-]>", function()
	local line = vim.api.nvim_get_current_line()
	local regex = vim.regex([[\w\{0,1\}\s*\*]])
	local start, stop = regex:match_str(line)
	local title = line:sub(1, start + 1)
	if start == 0 then
		title = ""
	end
	local tag = line:sub(stop)
	local tw = vim.opt.textwidth:get() - 10
	local pad = vim.fn["repeat"](" ", tw - #title)
	local new_line = string.format("%s%s%s", title, pad, tag)
	vim.api.nvim_set_current_line(new_line)
	vim.api.nvim_win_set_cursor(0, {
		vim.api.nvim_win_get_cursor(0)[1],
		#new_line
	})
end, {
	buffer = true,
})
