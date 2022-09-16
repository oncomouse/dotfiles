if vim.fn.exists("g:todo_disable") == 0 then
	require("todo").setup()
	local todo_augroup = vim.api.nvim_create_augroup("todo", { clear = true })
	local function set_maps()
		vim.keymap.set("n", "gtd", "<Plug>(todo-toggle-done)", { buffer = true, silent = true, noremap = true })
		vim.keymap.set("n", "<leader>td", "<Plug>(todo-toggle-done-motion)", { buffer = true, silent = true, noremap = true })
		vim.keymap.set("x", "<leader>td", "<Plug>(todo-toggle-done-visual)", { buffer = true, silent = true, noremap = true })
		-- Go To Project:
		vim.keymap.set("n", "<leader>tg", "<Plug>(todo-goto-project)", { buffer = true, silent = true, noremap = true })
		-- Search For Done Tasks:
		vim.keymap.set("n", "<leader>t/", "<Plug>(todo-search-done)", { buffer = true, silent = true, noremap = true })
		-- Go To Next Project:
		vim.keymap.set({ "n", "v" }, "]t", "<Plug>(todo-next-project)", { buffer = true, silent = true, noremap = true })
		-- Go To Previous Project:
		vim.keymap.set({ "n", "v" }, "[t", "<Plug>(todo-prev-project)", { buffer = true, silent = true, noremap = true })
	end
	vim.api.nvim_create_autocmd("BufRead,BufNewFile", {
		group = todo_augroup,
		pattern = "todo.*",
		callback = set_maps,
	})
	vim.api.nvim_create_autocmd("FileType", {
		group = todo_augroup,
		pattern = "vimwiki,markdown",
		callback = set_maps,
	})
end
