vim.keymap.set({ "n", "i" }, "<C-R>", function()
	local tw = vim.opt.textwidth:get()
	local line = vim.api.nvim_get_current_line()
	local pad = vim.fn["repeat"](" ", tw - #line)
	vim.api.nvim_set_current_line(pad .. line)
end, {
	buffer = true,
	desc = "Right align text"
})
