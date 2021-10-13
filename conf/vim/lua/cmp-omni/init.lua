--luacheck: globals vim
local cmp = require("cmp")
local source = {}

source.new = function()
	return setmetatable({}, { __index = source })
end

source.is_available = function()
	return #vim.opt.omnifunc:get() > 0
end

source.get_keyword_pattern = function()
	return "%S*"
end

source.complete = function(_, _, callback)
	local start = vim.fn[vim.opt.omnifunc:get()](1, "")
	-- Did not find a key in this line:
	if start < 0 then
		callback({})
		return
	end
	local line_string = vim.fn.getline('.')
	local col = vim.fn.col('.')
	local results = vim.fn[vim.opt.omnifunc:get()](0, line_string:sub(start, col))

	local entries = {}
	for _, result in ipairs(results.words) do
		table.insert(entries, {
			label = result.word,
			kind = cmp.lsp.CompletionItemKind.Keyword,
			documentation = {
				value = result.info,
				kind = cmp.lsp.MarkupKind.PlainText,
			},
		})
	end
	callback(entries)
end

return source
