local utils = require("yop.utils")
require("yop").op_map({ "n", "v" }, "gs", function(lines, opts)
	-- We don't care about anything non alphanumeric here
	local sort_without_leading_space = function(a, b)
		-- true = a then b
		-- false = b then a
		local pattern = [[^%W*]]
		return string.gsub(a, pattern, "") < string.gsub(b, pattern, "")
	end
	if #lines == 1 then
		-- If only looking at 1 line, sort that line split by some char gotten from input
		local delimeter = utils.get_input("Delimeter: ")
		local split = vim.split(lines[1], delimeter, { trimempty = true })
		-- Remember! `table.sort` mutates the table itself
		table.sort(split, sort_without_leading_space)
		return { utils.join(split, delimeter) }
	else
		-- If there are many lines, sort the lines themselves
		table.sort(lines, sort_without_leading_space)
		return lines
	end
end)
