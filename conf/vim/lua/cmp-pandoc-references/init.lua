--luacheck: globals vim
local source = {}
local bib_parse = require("cmp-pandoc-references.references")

source.new = function()
	return setmetatable({}, { __index = source })
end

source.is_available = function()
	return vim.o.filetype == "pandoc" or vim.o.filetype == "markdown"
end

source.get_keyword_pattern = function()
	return "[@][A-Za-z_0-9.-:_/]*"
end

source.complete = function(self, params, callback)
	local lines = vim.api.nvim_buf_get_lines(self.bufnr, 0, -1, false)
	local line_string = params.context.cursor_line
	local col = params.context.cursor.col
	--- locate start of the key
	local start = col - 1
	while start > 0 and line_string:sub(start, start) ~= "@" do
		-- Not typing a key:
		if string.match(line_string:sub(start, start), "[A-Za-z_0-9.-:_/]") == nil then
			start = -3
			break
		end
		start = start - 1
	end
	-- Did not find a key in this line:
	if start < 0 then
		callback({})
		return
	end
	local entries = bib_parse(line_string:sub(start, #line_string), lines)
	if entries then
		self.items = entries
		callback(self.items)
	end
end

return source
