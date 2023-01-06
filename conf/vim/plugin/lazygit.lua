local function open_lazygit()
	if vim.fn.executable("lazygit") == 1 then
		vim.cmd([[tab split +term\ lazygit]])
		vim.keymap.set("n", "q", function()
			vim.cmd([[quit]])
		end, {
			buffer = true,
		})
		vim.api.nvim_feedkeys("i", "n", false)
	else
		vim.cmd([[echohl ErrorMsg
		echo 'lazygit was not found in $PATH'
		echohl NONE]])
	end
end

vim.api.nvim_create_user_command("LazyGit", open_lazygit, {})
