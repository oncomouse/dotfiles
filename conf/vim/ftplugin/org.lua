-- Set indent:
vim.opt_local.tabstop = 2
vim.opt_local.shiftwidth = 2
vim.opt_local.softtabstop = 2
vim.opt_local.expandtab = true
-- Turn on conceal:
vim.opt_local.conceallevel = 2
-- Spell
vim.opt_local.spell = true

-- Use org-lint for debugging:
vim.cmd([[compiler org-lint]])

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
vim.keymap.set(
	"i",
	"<C-c>.",
	"<cmd>lua require('orgmode').action('org_mappings.insertmode_date')<cr>",
	{ buffer = true }
)
vim.keymap.set(
	"i",
	"<C-c>!",
	"<cmd>lua require('orgmode').action('org_mappings.insertmode_date','true')<cr>",
	{ buffer = true }
)

-- Load markdown.nvim for org:
vim.b.markdown_nvim_unordered_list_chars = "[+-]"
vim.b.markdown_nvim_unordered_default = "+"
require("markdown").setup()

vim.api.nvim_buf_create_user_command(0, "ECMinutes", function()
	local bn = vim.api.nvim_buf_get_name(0)
	if type(bn) == "string" and string.match(bn, "meetings.*executive") then
		vim.cmd(
			string.format(
				"silent !pandoc -i %s -o %s/ec-minutes/ec-minutes-%s.docx",
				vim.fn.fnameescape(vim.fn.expand("%")),
				vim.fn.fnameescape(vim.fn.expand("%:h")),
				os.date("%Y-%m-%d")
			)
		)
		vim.print(
			string.format(
				"Created: %s/ec-minutes/ec-minutes-%s.docx",
				vim.fn.fnameescape(vim.fn.expand("%:h")),
				os.date("%Y-%m-%d")
			)
		)
	end
end, {
	force = true,
})
