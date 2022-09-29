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
	local lua_sub = pattern:gsub("^/", "")
	local lines = vim.api.nvim_buf_get_lines(0, 0, -1, true)
	while start >= 1 and string.match(lines[start], lua_sub) do
		start = start - 1
	end
	while stop <= #lines and string.match(lines[stop], lua_sub) do
		stop = stop + 1
	end
	start = start + 1
	stop = stop - 1
	lines = vim.list_slice(lines, start, stop)
	lines = require("mini.align").align_strings(lines, { split_pattern = lua_sub, merge_delimiter = " " }, nil)
	set_lines(start - 1, stop, lines)
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
