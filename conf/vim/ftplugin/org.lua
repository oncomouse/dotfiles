-- Set indent:
vim.opt_local.tabstop = 2
vim.opt_local.shiftwidth = 2
vim.opt_local.softtabstop = 2
vim.opt_local.expandtab = true

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

