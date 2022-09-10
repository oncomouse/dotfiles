local M = {}

function M.setmetatable(table)
	return setmetatable(table, {
		__index = function(t, idx)
			return t[idx] and t[idx] or function()
				return nil
			end
		end,
	})
end
return M
