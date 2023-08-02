-- Pandoc <format> to compile documents quickly and easily:
vim.api.nvim_create_autocmd("FileType", {
	group = vim.api.nvim_create_augroup("dotfiles-pandoc-cmd", {}),
	pattern = "markdown, org",
	callback = function(ev)
		vim.api.nvim_buf_create_user_command(ev.buf, "Pandoc", function(args)
			vim.cmd(
				"!pandoc -i "
				.. vim.fn.fnameescape(vim.fn.expand("%"))
				.. " -o "
				.. vim.fn.fnameescape(vim.fn.expand("%:r"))
				.. "."
				.. args.args
			)
		end, {
			nargs = 1,
		})
	end
})

