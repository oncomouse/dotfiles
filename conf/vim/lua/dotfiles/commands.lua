-- Formatting and Diagnostic commands for LSP-less files
vim.api.nvim_create_user_command("Diagnostics", function()
	vim.cmd("silent lmake! %")
	if #vim.fn.getloclist(0) == 0 then
		vim.cmd("lopen")
	else
		vim.cmd("lclose")
	end
end, {
	force = true,
})
vim.api.nvim_create_user_command("Format", function()
	vim.api.nvim_feedkeys("mxgggqG`x", "x", true)
end, {
	force = true,
})

-- Adjust Spacing:
vim.api.nvim_create_user_command("Spaces", function(args)
	local wv = vim.fn.winsaveview()
	vim.opt_local.expandtab = true
	vim.opt_local.tabstop = tonumber(args.args)
	vim.opt_local.softtabstop = tonumber(args.args)
	vim.opt_local.shiftwidth = tonumber(args.args)
	vim.cmd("silent execute '%!expand -it" .. args.args .. "'")
	vim.fn.winrestview(wv)
	vim.cmd("setlocal ts? sw? sts? et?")
end, {
	force = true,
	nargs = 1,
})
vim.api.nvim_create_user_command("Tabs", function(args)
	local wv = vim.fn.winsaveview()
	vim.opt_local.expandtab = false
	vim.opt_local.tabstop = tonumber(args.args)
	vim.opt_local.softtabstop = tonumber(args.args)
	vim.opt_local.shiftwidth = tonumber(args.args)
	vim.cmd("silent execute '%!unexpand -t" .. args.args .. "'")
	vim.fn.winrestview(wv)
	vim.cmd("setlocal ts? sw? sts? et?")
end, {
	force = true,
	nargs = 1,
})

-- :Git command
vim.api.nvim_create_user_command("Git", function(args)
	if args.args == "" then
		vim.cmd("LazyGit")
	end
	vim.cmd("!git " .. args.args)
end, {
	force = true,
	nargs = "*",
})

-- Mf for directory creation:
vim.api.nvim_create_user_command("Mf", function(args)
	vim.fn.system("mf " .. args.args)
	vim.cmd("e " .. args.args)
end, {
	complete = "dir",
	nargs = 1,
})

-- LazyGit integration:
vim.api.nvim_create_user_command("LazyGit", function()
	if vim.fn.executable("lazygit") == 1 then
		vim.cmd([[tab split +term\ lazygit]])
		vim.keymap.set("n", "q", function()
			vim.cmd([[quit]])
		end, {
			buffer = true,
		})
		vim.api.nvim_feedkeys("i", "n", false)
		vim.api.nvim_create_autocmd("TermClose", {
			buffer = vim.fn.bufnr(""),
			command = "if !v:event.status | exe 'bdelete! '..expand('<abuf>') | endif",
		})
	else
		vim.cmd([[echohl ErrorMsg
		echo 'lazygit was not found in $PATH'
		echohl NONE]])
	end
end, {})

-- Tabularize:
-- Taken from mini.align's source code:
function set_lines(start_row, end_row, replacement)
	--stylua: ignore
	local cmd = string.format(
		'lockmarks lua vim.api.nvim_buf_set_lines(0, %d, %d, true, %s)',
		start_row, end_row, vim.inspect(replacement)
	)
	vim.cmd(cmd)
end

-- Drop-in, basic replacement for godlygeek/tabular using mini.align
-- This does not support full tabular formatting, /<regex>/(r|c|l)[0-9]. It is
-- only intended to support the `:TableFormat` command in 
-- preservim/vim-markdown (which calls `:Tabularize /|`).
local function tabular(pattern, start, stop)
	-- Extract region
	local lua_sub, _ = unpack(vim.fn.split(pattern:gsub("^/", ""), "/")) -- second option holds formatting
	local lines = vim.api.nvim_buf_get_lines(0, 0, -1, true)
	while start >= 1 and string.match(lines[start], lua_sub) do
		start = start - 1
	end
	while stop <= #lines and string.match(lines[stop], lua_sub) do
		stop = stop + 1
	end
	start = start + 1
	stop = stop - 1
	if start <= stop then
		lines = vim.list_slice(lines, start, stop)
		lines = require("mini.align").align_strings(lines, { split_pattern = lua_sub, merge_delimiter = "" }, nil)
		set_lines(start - 1, stop, lines)
	end
end

vim.api.nvim_create_user_command("Tabularize", function(args)
	local pattern = args.fargs[1]
	assert(pattern ~= nil, "No pattern provided.")
	local start = args.line1
	local stop = args.line2
	tabular(pattern, start, stop)
end, {
	range = true,
	nargs = "*",
})
