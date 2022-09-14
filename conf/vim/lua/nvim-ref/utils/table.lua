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

function M.append(...)
	local args = {...}
	assert(#args >= 2, "Arguments to nvim-ref.utils.table must be greater than two!")
	local target = args[1]
	assert(type(target) == "table", "First argument to nvim-ref.utils.table must be a table!")
	table.remove(args, 1)
	for _,addon in pairs(args) do
		if type(addon) ~= "table" then
			addon = { addon }
		end
		for _,x in pairs(addon) do
			table.insert(target, x)
		end
	end
	return target
end

return M
