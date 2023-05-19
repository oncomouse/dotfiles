-- Local settings for Markdown
vim.opt_local.wrap = true
vim.opt_local.linebreak = true
vim.opt_local.list = false
vim.opt_local.spell = true
vim.opt_local.showbreak = "NONE"

-- Turn conceal on and off in a buffer:
vim.keymap.set("n", "<leader>cc", function()
	vim.opt_local.conceallevel = vim.opt_local.conceallevel == 0 and 2 or 0
end, {
	buffer = true,
	silent = true,
})

-- Remap gd to follow footnotes
vim.keymap.set("n", "gd", "<Plug>(markdown-nvim-footnote)", { buffer = true })

vim.cmd([[compiler markdown_combo]])

-- vim.opt_local.iskeyword = vim.opt_local.iskeyword + "',-,@-@"

-- Pandoc <format> to compile documents quickly and easily:
vim.api.nvim_create_user_command("Pandoc", function(args)
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
