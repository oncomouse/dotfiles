return {
		"zdcthomas/yop.nvim",
		keys = {
			{ "gs", mode = { "n", "v" } },
		},
		config = function()
			local utils = require("yop.utils")
			local function sort_without_leading_space(a, b)
				-- true = a then b
				-- false = b then a
				local pattern = [[^%W*]]
				return string.gsub(a, pattern, "") < string.gsub(b, pattern, "")
			end
			local function sort_by_first_line(ca, cb)
				return sort_without_leading_space(ca[1], cb[1])
			end
			local function sort_by_delimiter(line)
				-- If only looking at 1 line, sort that line split by some char gotten from input
				local delimeter = utils.get_input("Delimeter: ")
				local split = vim.split(line, delimeter, { trimempty = true })
				-- Remember! `table.sort` mutates the table itself
				table.sort(split, sort_without_leading_space)
				return { utils.join(split, delimeter) }
			end
			require("yop").op_map({ "n", "v" }, "gs", function(lines)
				-- We don't care about anything non alphanumeric here
				if #lines == 1 then
					return sort_by_delimiter(lines[1])
				end
				-- If there are many lines, sort the lines themselves
				table.sort(lines, sort_without_leading_space)
				return lines
			end)
			-- Chunkwise sort (works by sorting via the first line of a paragraph)
			-- require("yop").op_map({ "n", "v" }, "gS", function(lines)
			-- 	if #lines == 1 then
			-- 		return sort_by_delimiter(lines[1])
			-- 	end
			-- 	local chunks = {}
			-- 	local current_chunk = {}
			-- 	for _, line in pairs(lines) do
			-- 		if #line == 0 and #current_chunk > 0 then
			-- 			table.insert(chunks, current_chunk)
			-- 			current_chunk = {}
			-- 		else
			-- 			table.insert(current_chunk, line)
			-- 		end
			-- 	end
			-- 	if #current_chunk > 0 then
			-- 		table.insert(chunks, current_chunk)
			-- 	end
			-- 	vim.pretty_print(chunks)
			-- 	if #chunks == 1 then
			-- 		return lines
			-- 	end
			-- 	table.sort(chunks, sort_by_first_line)
			-- 	local output = {}
			-- 	for _, chunk in pairs(chunks) do
			-- 		for _, line in pairs(chunk) do
			-- 			table.insert(output, line)
			-- 		end
			-- 		table.insert(output, "")
			-- 	end
			-- 	return output
			-- end)
		end,
	}
