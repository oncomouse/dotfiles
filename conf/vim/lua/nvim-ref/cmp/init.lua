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

source.complete = function(_, params, callback)
	local items = {}

	local start = require("nvim-ref.filetypes").require().find_start()
	if start ~= nil then
		-- Clean target:
		local target = params.context.cursor_before_line:sub(start):match("[^, \t{}@}\n]*$")
		local entries = require("nvim-ref.bibliography").query(target)
		for _,entry in pairs(entries) do
			table.insert(items, require("nvim-ref.utils.lsp").make_lsp_item(entry))
		end
	end

	callback({ items = items })
end

return source
