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
for _, lhs in pairs(vim.tbl_keys(translation)) do
	vim.keymap.set("n", lhs, function()
		local line = vim.api.nvim_get_current_line()
		-- If line is a headline, promote/demote using orgmode:
		if line:match("^%*+") then
			require("orgmode").action("org_mappings.do_" .. translation[lhs])
		else
			vim.cmd("exe 'normal! " .. vim.v.count1 .. lhs .. "'")
		end
	end, {
		buffer = true,
		desc = "Promote/Demote or indent/undent",
	})
end

-- Map org-mode's <M-CR> behavior into nvim-orgmode
vim.keymap.set("i", "<M-CR>", "<cmd>lua require('orgmode').action('org_mappings.handle_return')<cr>", { buffer = true })

-- Use old todo manager mapping for checkbox toggles:
vim.keymap.set("n", "gtd", "<cmd>lua require('orgmode').action('org_mappings.toggle_checkbox')<cr>", { buffer = true })

