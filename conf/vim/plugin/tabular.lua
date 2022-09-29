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
local function tabular(pattern, start, stop)
	-- Extract region
	local lua_sub = pattern:gsub("^/", "")
	start = start - 1
	stop = stop - 1
	while start >= 0 and string.match(vim.api.nvim_buf_get_lines(0, start, start + 1, false)[1], lua_sub) do
		start = start - 1
	end
	while stop < vim.api.nvim_buf_line_count(0) and string.match(vim.api.nvim_buf_get_lines(0, stop, stop + 1, false)[1], lua_sub) do
		stop = stop + 1
	end
	local region = {
		start = {
			col = 0,
			line = start + 1,
		},
		stop = {
			col = #(vim.api.nvim_buf_get_lines(0, stop - 1, stop, false)[1]),
			line = stop,
		}
	}
	local lines = vim.api.nvim_buf_get_lines(0, region.start.line, region.stop.line, false)
	lines = require("mini.align").align_strings(lines, { split_pattern = lua_sub, merge_delimiter = " " }, nil)
	set_lines(region.start.line, region.stop.line, lines)
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

