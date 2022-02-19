vim.api.nvim_add_user_command("Mf", function(args)
	vim.fn.system("mf " .. args.args)
	vim.cmd("e " .. args.args)
end, {
	complete = "dir",
	nargs = 1,
})
