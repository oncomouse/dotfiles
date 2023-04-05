EntireTextobject = function(visual)
	vim.cmd("normal! m'")
	vim.cmd("keepjumps normal! gg0")
	if not visual then
		vim.cmd("normal! V")
	end
	vim.cmd("normal! o")
	vim.cmd("keepjumps normal! G$")
end

vim.keymap.set("o", "ae", "<cmd>call v:lua.EntireTextobject(v:false)<cr>")
vim.keymap.set("o", "ie", "<cmd>call v:lua.EntireTextobject(v:false)<cr>")
vim.keymap.set("x", "ae", "<cmd>call v:lua.EntireTextobject(v:true)<cr>")
vim.keymap.set("x", "ie", "<cmd>call v:lua.EntireTextobject(v:true)<cr>")
