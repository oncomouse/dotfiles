if vim.g.loaded_fzf_lua == 1 then
	-- Custom :Git command to utilize fzf-lua for status
	vim.api.nvim_create_user_command("Git", function(args)
		if args.args:match("^status") then
			vim.cmd("GitStatus")
		else
			vim.cmd("Gina " .. args.args)
		end
	end, {
		force = true,
		nargs = "+",
	})
end
