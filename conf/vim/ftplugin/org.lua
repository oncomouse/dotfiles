-- Set indent:
vim.opt_local.tabstop = 2
vim.opt_local.shiftwidth = 2
vim.opt_local.softtabstop = 2
vim.opt_local.expandtab = true
-- Turn on conceal:
vim.opt_local.conceallevel = 2
-- Spell
vim.opt_local.spell = true

-- Do a better >>/<< for my editing style
local translation = {
	["<<"] = "promote",
	[">>"] = "demote",
}
local function do_demote_promote(symbol)
	local line = vim.api.nvim_get_current_line()
	if line:match("^%*+") then
		require("orgmode").action("org_mappings.do_" .. translation[symbol])
	else
		vim.cmd("exe 'normal! " .. vim.v.count1 .. symbol .. "'")
	end
end
vim.keymap.set("n", "<<", function()
	do_demote_promote("<<")
end, {
	buffer = true,
})
vim.keymap.set("n", ">>", function()
	do_demote_promote(">>")
end, {
	buffer = true,
})

-- Map org-mode's <M-CR> behavior into nvim-orgmode
vim.keymap.set("i", "<M-CR>", "<c-o><leader><cr>", { buffer = true, remap = true })

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
