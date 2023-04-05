EntireTextobject = {}

EntireTextobject.outer = function(visual)
	vim.cmd("normal! m'")
	vim.cmd("keepjumps normal! gg0")
	if not visual then
		vim.cmd("normal! V")
	end
	vim.cmd("normal! o")
	vim.cmd("keepjumps normal! G$")
end

EntireTextobject.inner = function(visual)
	vim.cmd("normal! m'")
	vim.cmd("keepjumps normal! gg0")
	vim.fn.search("^.", "cW")
	if not visual then
		vim.cmd("normal! V")
	end
	vim.cmd("normal! o")
	vim.cmd("keepjumps normal! G$")
	vim.fn.search("^.", "bcW")
	vim.cmd("normal! $")
end

vim.keymap.set("o", "ae", "<cmd>lua EntireTextobject.outer(false)<cr>")
vim.keymap.set("o", "ie", "<cmd>lua EntireTextobject.inner(false)<cr>")
vim.keymap.set("x", "ae", "<cmd>lua EntireTextobject.outer(true)<cr>")
vim.keymap.set("x", "ie", "<cmd>lua EntireTextobject.inner(true)<cr>")
