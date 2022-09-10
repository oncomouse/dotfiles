local M = {}

-- Source: https://developer.roblox.com/en-us/articles/Cloning-tables

function M.copy(original)
	local copy = {}
	for key, value in pairs(original) do
		copy[key] = value
	end
	return copy
end

function M.deep_copy(original)
	local copy = {}
	for k, v in pairs(original) do
		if type(v) == "table" then
			v = M.deep_copy(v)
		end
		copy[k] = v
	end
	return copy
end
-- End Source

return M
