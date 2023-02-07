-- LazyGit integration:
local function lazygit()
	if vim.fn.executable("lazygit") == 1 then
		vim.cmd([[tab split +term\ lazygit]])
		vim.keymap.set("n", "q", function()
			vim.cmd([[quit]])
		end, {
			buffer = true,
		})
		vim.api.nvim_feedkeys("i", "n", false)
		vim.api.nvim_create_autocmd("TermClose", {
			buffer = vim.fn.bufnr(""),
			command = "if !v:event.status | exe 'bdelete! '..expand('<abuf>') | endif",
		})
	else
		vim.cmd([[echohl ErrorMsg
		echo 'lazygit was not found in $PATH'
		echohl NONE]])
	end
end
vim.api.nvim_create_user_command("LazyGit", lazygit, {})


