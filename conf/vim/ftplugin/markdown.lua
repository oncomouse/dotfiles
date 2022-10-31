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
local function match_all_lines(lines, pattern)
	local test = nil
	for _,line in pairs(lines) do
		test = line:match(pattern)
		if not test then
			return false, nil
		end
	end
	return true, test
end
local patterns = {
	"^> ",
	"^[0-9]+%. ",
	"^[*-] ",
}
vim.keymap.set("n", "J", function()
	local linenr = vim.fn.line(".") - 1
	local start, next = unpack(vim.api.nvim_buf_get_lines(0, linenr, linenr + 2, false))
	for _, pattern in pairs(patterns) do
		local matches, match = match_all_lines({ start, next}, pattern)
		if matches then
			next = next:sub(#match + 1)
			break
		end
	end
	vim.api.nvim_buf_set_lines(0, linenr, linenr + 2, false, {
		start .. " " .. next
	})
end, {
	buffer = true,
})

-- Continue markdown markup on "o" and "O"
