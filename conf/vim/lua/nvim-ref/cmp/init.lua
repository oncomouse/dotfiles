local M = {}

function M.register()
	local ok, cmp = pcall(require, "cmp")
	if ok then
		cmp.register_source("nvim_ref", require("nvim-ref.cmp").new())
	end
end

M.new = function()
	return setmetatable({}, { __index = M })
end

M.is_available = function()
	return vim.b.nvim_ref_loaded
end

M.get_keyword_pattern = function()
	return [[\k\+]]
end

M.complete = function(_, params, callback)
	local items = {}

	local start = require("nvim-ref.filetypes").require().find_start()
	if start ~= nil then
		-- Clean target:
		local target = params.context.cursor_before_line:sub(start):match("[^, \t{}@}\n]*$")
		local entries = require("nvim-ref.bibliography").query(target)
		for _, entry in pairs(entries) do
			table.insert(items, require("nvim-ref.utils.lsp").make_lsp_item(entry))
		end
	end

	callback({ items = items })
end

return M
