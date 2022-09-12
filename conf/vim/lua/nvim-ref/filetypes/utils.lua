local M = {}

function M.setmetatable(table)
	return setmetatable(table, {
		__index = function(t, idx)
			if idx == "find_start" then
				return function()
					return require("nvim-ref.filetypes").find_start(t.start_pattern or [[\k\+]])
				end
			end
			return t[idx] and t[idx] or function()
				return nil
			end
		end,
	})
end
return M
