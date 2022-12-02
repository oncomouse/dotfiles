if vim.fn.exists("g:todo_disable") == 0 then
	vim.keymap.set("", "<Plug>(todo-next-project)", ":<C-u>lua require('todo').find_project('next')<CR>")
	vim.keymap.set("", "<Plug>(todo-prev-project)", ":<C-u>lua require('todo').find_project('prev')<CR>")
	vim.keymap.set("", "<Plug>(todo-search-done)", [[/\(^\s*[-*] \[[xX]\].*$\|^.* X$\)<CR>]])
	vim.keymap.set("", "<Plug>(todo-toggle-done)", "v:lua.require'todo'.operatorfunc() . '$'", { expr = true })
	vim.keymap.set("", "<Plug>(todo-toggle-done-visual)", ":<C-u>lua require('todo').operatorfunc('visual')<CR>")
	vim.keymap.set("", "<Plug>(todo-toggle-done-motion)", "v:lua.require'todo'.operatorfunc()", { expr = true })
	vim.keymap.set("", "<Plug>(todo-goto-project)", ":<C-u>lua require('todo').goto_project()<CR>")

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
