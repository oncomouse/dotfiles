EntireTextobject = {}

EntireTextobject = function(visual, inner)
	vim.cmd("normal! m'")
	vim.cmd("keepjumps normal! gg0")
	if inner then
		vim.fn.search("^.", "cW")
	end
	if not visual then
		vim.cmd("normal! V")
	end
	vim.cmd("normal! o")
	vim.cmd("keepjumps normal! G$")
	if inner then
		vim.fn.search("^.", "bcW")
		vim.cmd("normal! $")
	end
end

vim.keymap.set("o", "ae", "<cmd>lua EntireTextobject(false)<cr>")
vim.keymap.set("o", "ie", "<cmd>lua EntireTextobject(false, true)<cr>")
vim.keymap.set("x", "ae", "<cmd>lua EntireTextobject(true)<cr>")
vim.keymap.set("x", "ie", "<cmd>lua EntireTextobject(true, true)<cr>")
