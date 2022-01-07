-- luacheck: globals vim
-- Turn off FZF trigger for easier QF navigation:
vim.keymap.set("n", "<C-P>", "<C-P>", { buffer = true }) -- nmap <buffer> <C-P> <C-P>
local status, _ = pcall(require, "qf_helper")
if status then
	vim.keymap.set("n", "<C-s>", function()
		require("qf_helper").open_split("split")
	end, {
		silent = true,
		noremap = true,
		buffer = true,
	})
	-- Open in a vertical split:
	vim.keymap.set("n", "<C-v>", function()
		require("qf_helper").open_split("vsplit")
	end, {
		silent = true,
		noremap = true,
		buffer = true,
	})
	-- Open without leaving quickfix:
	vim.keymap.set("n", "<C-y>", "<CR><C-W>p", { silent = true, noremap = true, buffer = true })
	vim.keymap.set("n", "<C-j>", "j<CR><C-W>p", { silent = true, noremap = true, buffer = true })
	vim.keymap.set("n", "<C-k>", "k<CR><C-W>p", { silent = true, noremap = true, buffer = true })
	vim.keymap.set("n", "{", function()
		require("qf_helper").navigate(-1, { by_file = true })
		vim.cmd("wincmd p")
	end, {
		silent = true,
		noremap = true,
		buffer = true,
	})
	vim.keymap.set("n", "}", function()
		require("qf_helper").navigate(1, { by_file = true })
		vim.cmd("wincmd p")
	end, {
		silent = true,
		noremap = true,
		buffer = true,
	})
end
