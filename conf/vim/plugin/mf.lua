-- Mf for directory creation:
vim.api.nvim_create_user_command("Mf", function(args)
	local file = vim.loop.fs_realpath(args.args) or args.args
	vim.fn.mkdir(vim.fn.fnamemodify(file, ":p:h"), "p")
	vim.cmd("e " .. file)
end, {
	complete = "dir",
	nargs = 1,
})
