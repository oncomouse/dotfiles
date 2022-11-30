if vim.fn.exists("g:todo_disable") == 0 then
	local todo_augroup = vim.api.nvim_create_augroup("todo", { clear = true })
	vim.api.nvim_create_autocmd("BufRead,BufNewFile", {
		group = todo_augroup,
		pattern = "todo.*",
		callback = require("todo").set_maps,
	})
	vim.api.nvim_create_autocmd("FileType", {
		group = todo_augroup,
		pattern = "vimwiki,markdown",
		callback = require("todo").set_maps,
	})
end
