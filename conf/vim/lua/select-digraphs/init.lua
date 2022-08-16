-- This is better-digraphs.nvim with telescope.nvim replaced with vim.ui.select
--   (original is at: https://github.com/protex/better-digraphs.nvim)

local get_cursor_column = function()
	local _, col = unpack(vim.api.nvim_win_get_cursor(0))
	return col
end

local match_digraph_table_header = function(line)
	return string.match(line, "official name")
end

local is_empty_string = function(line)
	return line == ""
end

local match_digraph_table_footer = function(line)
	return string.match(line, "vim:tw=78:ts=8:noet:ft=help:norl:")
end

local get_digraph_from_doc = function()
	local digraph_doc = vim.fn.expand("$VIMRUNTIME/doc/digraph.txt")
	local ok, f_lines = pcall(io.lines, digraph_doc)
	if ok then
		local lines = {}
		local line_number = 1
		local table_found = false
		for line in f_lines do
			if string.match(line, "digraph%-table%-mbyte") then
				table_found = true
				line_number = 1
			elseif
				table_found
				and not match_digraph_table_header(line)
				and not is_empty_string(line)
				and not match_digraph_table_footer(line)
			then
				lines[line_number] = line
				line_number = line_number + 1
			end
		end
		return lines
	end
	return {}
end

local generate_default_digraphs = function()
	local digraph_raw_list = get_digraph_from_doc()
	return vim.tbl_map(function(line)
		local columns = vim.fn.split(line, "\t")
		return { columns[5], columns[2], columns[1] }
	end, digraph_raw_list)
end

local digraphs = nil

local function select_digraph(mode)
	mode = mode or "i"

	-- Load digraphs and strip off the ones that don't come out of the parser correctly:
	if digraphs == nil then
		digraphs = vim.tbl_filter(function(item)
			return #item == 3 and item[1] ~= nil and item[2] ~= nil and item[3] ~= nil
		end, generate_default_digraphs())
	end

	vim.ui.select(digraphs, {
		prompt = "Digraph: ",
		format_item = function(item)
			return item[1] .. " (" .. item[2] .. "): " .. item[3]
		end,
	}, function(choice)
		if choice == nil then
			if mode == "i" then -- Restore input
				vim.api.nvim_feedkeys("i", "", false)
			end
			return
		end

		if string.match(mode, "^i$") then
			if vim.fn.mode() ~= "i" then
				if get_cursor_column() == (#vim.api.nvim_get_current_line() - 1) then
					vim.api.nvim_feedkeys("a", "", false)
				else
					vim.api.nvim_feedkeys("i", "", false)
				end
			end
			vim.api.nvim_feedkeys("" .. choice[2], "", false)
		elseif string.match(mode, "^r$") then
			vim.api.nvim_feedkeys("r" .. choice[2], "", false)
		elseif string.match(mode, "^gvr$") then
			vim.api.nvim_feedkeys("gvr" .. choice[2], "", false)
		end
	end)
end

return select_digraph
