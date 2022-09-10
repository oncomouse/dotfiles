local source = {}

source.new = function()
	return setmetatable({}, { __index = source })
end

source.is_available = function()
	return vim.b.nvim_ref_loaded
end

source.get_keyword_pattern = function()
	return [[\k\+]]
end

source.complete = function(self, params, callback)
	print(vim.inspect(params))
	local items = {}
	--  table.insert(items, {
	-- label = v.abbr or v.word,
	-- insertText = v.word,
	-- labelDetails = {
	--   detail = v.kind,
	--   description = v.menu,
	-- },
	--  })
	callback({ items = items })
end

return source
