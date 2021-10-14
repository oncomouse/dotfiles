-- luacheck: globals vim
local function vsnip_completefunc(findstart, base)
	if findstart == 1 then
		local line_string = vim.fn.getline('.')
		local col = vim.fn.col('.')
		local start = col
		while start > 0 do
			-- Not typing a key:
			if string.match(line_string:sub(start, start), '%s') then
				break
			end
			start = start - 1
		end
		-- Did not find a key in this line:
		if start < 0 then
			start = -3
		end
		return start
	end
	local completion_items = { words = {} }
	local targets = vim.tbl_filter(function(item)
		return string.match(item.word, "^" .. base)
	end, vim.fn['vsnip#get_complete_items'](vim.api.nvim_get_current_buf()))
	for _, item in ipairs(targets) do
		table.insert(completion_items.words, item)
	end
	return completion_items
end
return vsnip_completefunc
