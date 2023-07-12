-- Local settings for Markdown
vim.opt_local.wrap = true
vim.opt_local.linebreak = true
vim.opt_local.list = false
vim.opt_local.spell = true
vim.opt_local.showbreak = "NONE"
vim.opt_local.omnifunc = "v:lua.require'nvim-ref.omnifunc'"

-- Remap gd to follow footnotes
vim.keymap.set("n", "gd", "<Plug>(markdown-nvim-footnote)", { buffer = true })

vim.cmd([[compiler markdown_combo]])

-- This messes up null-ls completion if uncommented:
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

if not vim.g.nvim_ref_registered then
	vim.g.nvim_ref_registered = true
	require("null-ls").register({
		require("nvim-ref.null-ls.hover"),
		require("nvim-ref.null-ls.completion"),
	})
end
