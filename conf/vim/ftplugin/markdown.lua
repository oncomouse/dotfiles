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

-- Markdown helper maps:

-- Remove markdown markup when joining line
vim.keymap.set({ "n" }, "J", require("dotfiles.markdown").join, { buffer = true })
vim.keymap.set({ "v" }, "J", require("dotfiles.markdown").join_opfunc, { expr = true, buffer = true })

-- Continue markdown markup on "o" and "O"
